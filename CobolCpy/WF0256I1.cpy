000100 01  REQU-WF0256I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0256             
000300*                                 PERIOD CALENDER MAINTENANCE             
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-DASTADAT-KEY    PIC X(8).                                    
000800*                                 GENERELLT STARTDATUM                    
000900*                                 GENERAL START DATE                      
001000     03 REQU-TIRP            PIC 9(2).                                    
001100*                                 REDOVISNINGSPERIOD                      
001200*                                 12 PER ≈R                               
001300*                                 ACCOUNTING PERIOD                       
001400*                                 12 PER YEAR                             
001500     03 REQU-DAFINDOC        PIC X(8).                                    
001600*                                 DOKUMENT DATUM (≈≈≈≈MMDD)               
001700*                                 INVOICING DATE   (YYYYMMDD)             
001800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
