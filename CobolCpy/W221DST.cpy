000100 01  W221DST.                                                             
000200*                                 ODETTE-SEGMENT DST                      
000300*                                 DELIVERY STATUS                         
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '028'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 TIYYMMDD-AVST        PIC 9(6).                                    
001000*                                 DATUM FÖR LEVERANSPLAN TAG 2253         
001010     03 FILLER               PIC X(1) VALUE SPACE.                        
001020*                                 FILLER                                  
001100     03 KVINLAAR             PIC S9(10).                                  
001300*                                 ACK INLEV DETTA ÅR     TAG 6804         
001400     03 FILLER               PIC X(1) VALUE SPACE.                        
001500*                                 FILLER                                  
001800     03 KVART-BREST          PIC S9(10).                                  
001900*                                 BESTÄLLNINGSREST       TAG 6812         
001910     03 KVART-BREST-ALPHA    REDEFINES KVART-BREST                        
001920                             PIC X(10).                                   
002000*** END COPY W221DST     LENGTH=34    OLD LENGTH=34                       
