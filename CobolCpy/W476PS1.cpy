000100 01  HDR-W476PS1.                                                         
000200*                                 COPYTEXT FOR PACKING SPEC HEADE         
000300*                                 R WEB-LDC                               
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
001500     03 HDR-RAD4H-TIYYMMDD   PIC 9(6).                                    
001600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001700     03 HDR-RAD4H-IDDISTR    PIC Z(4)9.                                   
001800*                                 DISTRIKTNUMMER                          
001900     03 HDR-RAD4H-IDSHIPM    PIC Z(6)9.                                   
002000*                                 SKEPPNINGSNUMMER                        
002100     03 HDR-RAD4H-IDTRPTNR   PIC Z(2)9.                                   
002200*                                 TRANSPORTIDENTITET                      
002300     03 HDR-RAD4H-IDLBBET    PIC X(12).                                   
002400*                                 LASTBÄRARBETECKNING                     
002500*** END OF VILMAII-COPY LENGTH= 233 BYTES                                 
