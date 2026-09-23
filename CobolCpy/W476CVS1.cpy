000100 01  HDR-W476CVS1.                                                        
000200*                                 COPYTEXT FOR CARGO VALUE SPEC H         
000300*                                 EADER WEB-LDC                           
000400     03 HDR-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 HDR-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 HDR-RAD1H-IMPORTER-TEXT                                           
000900                             PIC X(13).                                   
001000     03 HDR-RAD1H-IMPORTER   PIC X(35).                                   
001100     03 HDR-RAD2H-IMPORTER   PIC X(35).                                   
001200     03 HDR-RAD3H-IMPORTER   PIC X(35).                                   
001300     03 HDR-RAD4H-IMPORTER   PIC X(35).                                   
001400     03 HDR-RAD5H-IMPORTER   PIC X(35).                                   
001500     03 HDR-RAD1-TIAAMMDD    PIC 9(6).                                    
001600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001700     03 HDR-RAD1-IDDISTR     PIC Z(3)9.                                   
001800*                                 DISTRIKTNUMMER                          
001900     03 HDR-RAD1-IDSHIPM     PIC Z(6)9.                                   
002000*                                 SKEPPNINGSNUMMER                        
002100     03 HDR-RAD1-IDTRPTNR    PIC Z(2)9.                                   
002200*                                 TRANSPORTIDENTITET                      
002300     03 HDR-RAD1-IDLBBET     PIC X(12).                                   
002400*                                 LASTBÄRARBETECKNING                     
002500*** END OF VILMAII-COPY LENGTH= 232 BYTES                                 
