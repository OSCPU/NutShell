module CacheChecker #(
    TAG_WIDTH = 39
) (
  input                         clk,
  input                         cacheHit,
  input                         mmio,
  input                         miss,
  input                         probe,
  input                         memReqValid,
  input                         stage3MetaValid,
  input [ TAG_WIDTH - 1 : 0 ]   stage3MetaTag,
  input                         stage3MetaDirty,
  input [1 : 0]                 flush,
  input [3 : 0]                 stage3MainState
);

    logic hit = cacheHit;
    property dont_hit_when_mmio;
        @(posedge clk) !(mmio && cacheHit);
    endproperty 

    logic memReadReqCond = miss && !flush[1] && stage3MetaDirty;
    assert_dont_hit_when_mmio:
        assert property(dont_hit_when_mmio) else $finish;  

    property dont_hit_when_miss;
        @(posedge clk) (hit |-> !miss) and (miss |-> !hit);
    endproperty
    assert_dont_hit_when_miss:
        assert property(dont_hit_when_miss) else $finish;

    sequence no_mmio_and_probe;
        !mmio && !probe;
    endsequence

    sequence read_miss_state_1;
        miss && $past((stage3MainState == 4'b0 && memReadReqCond),2) && $past(memReqValid);
    endsequence

    sequence read_miss_state_2;
        $past(stage3MainState) == 4'd1 && stage3MainState == 4'd2;
    endsequence

    property read_miss_check;
        no_mmio_and_probe ##0 read_miss_state_1 |-> read_miss_state_2;
    endproperty
    
endmodule
