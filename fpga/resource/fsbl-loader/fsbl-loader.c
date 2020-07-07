/******************************************************************************
*
* Copyright (C) 2013 - 2015 Xilinx, Inc.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
* XILINX  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
* WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
* OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*
* Except as contained in this notice, the name of the Xilinx shall not be used
* in advertising or otherwise to promote the sale, use or other dealings in
* this Software without prior written authorization from Xilinx.
*
******************************************************************************/
/*****************************************************************************/
/**
*
* @file xilffs_polled_example.c
*
*
* @note This example uses file system with SD to write to and read from
* an SD card using ADMA2 in polled mode.
* To test this example File System should not be in Read Only mode.
* To test this example USE_MKFS option should be true.
*
* This example was tested using SD2.0 card and eMMC (using eMMC to SD adaptor).
*
* To test with different logical drives, drive number should be mentioned in
* both FileName and Path variables. By default, it will take drive 0 if drive
* number is not mentioned in the FileName variable.
* For example, to test logical drive 1
* FileName =  "1:/<file_name>" and Path = "1:/"
* Similarly to test logical drive N, FileName = "N:/<file_name>" and
* Path = "N:/"
*
* None.
*
* <pre>
* MODIFICATION HISTORY:
*
* Ver   Who Date     Changes
* ----- --- -------- -----------------------------------------------
* 1.00a hk  10/17/13 First release
* 2.2   hk  07/28/14 Make changes to enable use of data cache.
* 2.5   sk  07/15/15 Used File size as 8KB to test on emulation platform.
* 2.9   sk  06/09/16 Added support for mkfs.
* 3.10  mn  08/18/18 Change file size to 8MB from 8KB for ZynqMP platform
*
*</pre>
*
******************************************************************************/

/***************************** Include Files *********************************/

#include "xparameters.h"	/* SDK generated parameters */
#include "xsdps.h"		/* SD device driver */
#include "xil_printf.h"
#include "ff.h"
#include "xil_cache.h"
#include "xplatform_info.h"
#include "xil_mmu.h"

/************************** Constant Definitions *****************************/


/**************************** Type Definitions *******************************/

/***************** Macros (Inline Functions) Definitions *********************/

/************************** Function Prototypes ******************************/
int FfsSdPolledExample(void);

/************************** Variable Definitions *****************************/
static FIL fil;		/* File object */
static FATFS fatfs;
/*
 * To test logical drive 0, FileName should be "0:/<File name>" or
 * "<file_name>". For logical drive 1, FileName should be "1:/<file_name>"
 */
static char FileName[32] = "RV_BOOT.bin";
static char *SD_File;

#define RV_DRAM_ENTRY	0x10000000
#define GPIO_BASE   	0x40000000

#define FILE_SIZE_MB	32

static UINT FileSize = (FILE_SIZE_MB * 1024 * 1024);

/*****************************************************************************/
/**
*
* Main function to call the SD example.
*
* @param	None
*
* @return	XST_SUCCESS if successful, otherwise XST_FAILURE.
*
* @note		None
*
******************************************************************************/
int main(void)
{
	int Status;
	
	volatile unsigned int *gpio_base = (void *)GPIO_BASE;

	int i;

	unsigned int sec_attr;

	INTPTR rv_dram_base;

	xil_printf("RISC-V Boot Environment Setup... \r\n");

	/* Disable cache for RV_DRAM_ENTRY */
	rv_dram_base = (INTPTR)RV_DRAM_ENTRY;

	//set uncached non-shareable section attribute
	sec_attr = 0x04de2;		//S=b0 TEX=b100 AP=b11 Domain=b1111 C=b0, B=b0

	for(i = 0; i < FILE_SIZE_MB; i++)
	{
		Xil_SetTlbAttributes(rv_dram_base, sec_attr);
		rv_dram_base += 0x100000;		//section size
	}

	/* Load RV_BOOT.bin file from SD card to RV_DRAM_ENTRY */
	Status = FfsSdPolledExample();
	if (Status != XST_SUCCESS) {
		xil_printf("SD Polled RISC-V boot file failed \r\n");
		return XST_FAILURE;
	}

	xil_printf("Successfully load RISC-V boot file into DRAM @ 0x%08x \r\n", RV_DRAM_ENTRY);

  if (gpio_base[2] & 0x1) {
    xil_printf("Pause due to SW0 on PYNQ is pulled up.\r\n");
    xil_printf("To continue, please pull down SW0.\r\n");
    while (gpio_base[2] & 0x1) ;
  }

  xil_printf("Reset RISC-V core.\r\n");
	/* release RISC-V reset signal */
	gpio_base[0] = 0x1;

	while(1);

	return XST_SUCCESS;

}

/*****************************************************************************/
/**
*
* File system example using SD driver to write to and read from an SD card
* in polled mode. This example creates a new file on an
* SD card (which is previously formatted with FATFS), write data to the file
* and reads the same data back to verify.
*
* @param	None
*
* @return	XST_SUCCESS if successful, otherwise XST_FAILURE.
*
* @note		None
*
******************************************************************************/
int FfsSdPolledExample(void)
{
	FRESULT Res;
	UINT NumBytesRead;

	/*
	 * To test logical drive 0, Path should be "0:/"
	 * For logical drive 1, Path should be "1:/"
	 */
	TCHAR *Path = "0:/";

	/*
	 * Register volume work area, initialize device
	 */
	Res = f_mount(&fatfs, Path, 0);

	if (Res != FR_OK) {
		return XST_FAILURE;
	}

	/*
	 * Open file with required permissions.
	 * Here - Creating new file with read/write permissions. .
	 * To open file with write permissions, file system should not
	 * be in Read Only mode.
	 */
	SD_File = (char *)FileName;

	xil_printf("open...\r\n");

	Res = f_open(&fil, SD_File, FA_READ);
	if (Res) {
		return XST_FAILURE;
	}

	xil_printf("lseek...\r\n");

	/*
	 * Pointer to beginning of file .
	 */
	Res = f_lseek(&fil, 0);
	if (Res) {
		return XST_FAILURE;
	}

	xil_printf("read...\r\n");

	/*
	 * Read data from file.
	 */
	Res = f_read(&fil, (void*)RV_DRAM_ENTRY, FileSize,
			&NumBytesRead);
	if (Res) {
		return XST_FAILURE;
	}

	xil_printf("close...\r\n");

	/*
	 * Close file.
	 */
	Res = f_close(&fil);
	if (Res) {
		return XST_FAILURE;
	}

	return XST_SUCCESS;
}
