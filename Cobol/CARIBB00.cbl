000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    CARI1000.                                                 
000300 AUTHOR.        CARINA HOLMQVIST.                                         
000400 DATE-WRITTEN.  JAN  2017.                                                
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
000800 DATA DIVISION.                                                           
000900 WORKING-STORAGE SECTION.                                                 
001000 77  PROGRAM-NAME                PIC X(8) VALUE 'TSTPGM00'.               
001100 01  GENERAL-CONSTANTS.                                                   
001200     03  JA                      PIC X(1)    VALUE 'Y'.                   
001300     03  NEJ                     PIC X(1)    VALUE 'N'.                   
001400                                                                          
001500 01  DYNAMIC-SUBPROGRAMS.                                                 
001600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
001700     03  ISPLINK                 PIC X(8)    VALUE 'ISPLINK'.             
001800     03  DSNPGM10                PIC X(8)    VALUE 'DSNPGM10'.            
001900 01  RC                   PIC S9(9) COMP.                                 
002000     COPY W0023.                                                          
002100     -COPY W0023 -PRE CARINA                                              
002200     COPY BATCHLOG.                                                       
002300     -COPY BATCHLOG -PRE CARINA                                           
002400 LINKAGE SECTION.                                                         
002500                                                                          
002600 PROCEDURE DIVISION.                                                      
002700 MAIN SECTION.                                                            
002800     DISPLAY 'MAINPGM'                                                    
002900     GOBACK                                                               
003000     .                                                                    
