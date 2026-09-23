000100 01  001-W6113001.                                                        
000200*                                 URVAL FRÅN W6D1                         
000300*                                 POSTER TILL AK-BELÄGGNINGSLISTA         
000400*                                 PTYP 001 SKAPANDE TID FÖR FILEN         
000500*                                 /                                       
000600*                                 SELECTED EXTRACT FROM W6D1              
000700*                                 RECORDS TO AK WORK LOAD LISTING         
000800*                                 RECTYPE 001  FILE CREATION TIME         
000900*                                                    .                    
001000     03 001-IDPTYP           PIC X(3).                                    
001100*                                 POSTTYP                                 
001200*                                 RECORD TYPE                             
001300     03 001-TIAAMMDD         PIC 9(6).                                    
001400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001500*                                 YEAR - MONTH - DAY  (YYMMDD)            
001600     03 001-TIHHMMSS         PIC 9(6).                                    
001700*                                 TIM - MIN - SEK   (HHMMSS)              
001800*                                 HOUR - MINUTE - SEC (HHMMSS)            
001900*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
