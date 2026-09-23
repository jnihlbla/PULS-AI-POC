000100 01  RR-W6012A1.                                                          
000200*                                 CREATE UNLOADING REPORT -D&P            
000300     03 RR-IDLBBET           PIC X(12).                                   
000400*                                 LASTBÄRARBETECKNING                     
000500*                                 TRAILER NUMBER                          
000600     03 RR-ADINLOMR-LPL      PIC X(4).                                    
000700*                                 LOSSNINGSPLATS                          
000800*                                 UNLOADING AREA                          
000900     03 RR-IDLEVNR           PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 RR-IDFS              PIC X(8).                                    
001300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001400*                                 ADVICE NOTE NUMBER ODETTE               
001500     03 RR-IDARTNR           PIC Z(9).                                    
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800     03 RR-KVANTAL           PIC Z(6).                                    
001900*                                 ANTAL                                   
002000*                                 NUMBER                                  
002100     03 RR-KVKOLLI           PIC Z(5).                                    
002200*                                 ANTAL KOLLI                             
002300*                                 NBR OF CASES                            
002400     03 RR-BEFT              PIC Z(2).                                    
002500*                                 FÖRPACKNINGSTYP                         
002600*                                 PACKAGING TYPE                          
002700     03 RR-ADINLOMR          PIC X(4).                                    
002800*                                 INLEVERANSOMRÅDE                        
002900*                                 RECEIVING AREA                          
003000     03 RR-KVAVIS            PIC Z(7).                                    
003100*                                 AVISERAT ANTAL                          
003200*                                 QUANTITY NOTIFIED                       
003300     03 RR-KVAVIS-PRIO       PIC Z(7).                                    
003400*                                 BERÄKN PRIORITERAD KVANT TOT            
003500*                                 CALC PRIO QUANTITY TOT                  
003600     03 RR-FLKVROS           PIC X.                                       
003700*                                 RESTORDERSALDO                          
003800*                                 BACKORDER QUANTITY                      
003900     03 RR-FLKVAKAR          PIC X.                                       
004000*                                 FLAGGA KARANTÄN                         
004100*                                 FLAG QUARANTINE                         
004200     03 RR-KDFARLIG          PIC X.                                       
004300*                                 KOD FÖR FARLIGT GODS                    
004400*                                 DANGEROUS GOODS CODE                    
004500*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
