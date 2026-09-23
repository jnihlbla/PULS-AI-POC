000100 01  RR-W6019D1.                                                          
000200*                                 DATA FOR WEB RECEIVING REPORT           
000300*                                 W6019D-001                              
000400     03 RR-IDLOPNRM          PIC Z(8)9.                                   
000500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000600*                                 (0VVDLLLLK)                             
000700*                                 SERIAL NO RECEIVING REPORT              
000800*                                 (0WWDLLLLC)                             
000900     03 RR-BEART             PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100*                                 PART DESCRIPTION                        
001200     03 RR-IDARTNR           PIC Z(9).                                    
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 RR-IDLEVNR           PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800     03 RR-IDFS              PIC X(8).                                    
001900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002000*                                 ADVICE NOTE NUMBER ODETTE               
002100     03 RR-IDLOPNRM-1        PIC Z(8)9.                                   
002200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002300*                                 (0VVDLLLLK)                             
002400*                                 SERIAL NO RECEIVING REPORT              
002500*                                 (0WWDLLLLC)                             
002600     03 RR-TIAVIDAT          PIC 9(6).                                    
002700*                                 AVISERINGSDATUM (YYMMDD)                
002800*                                 ADVICE NOTE DATE                        
002900     03 RR-KVAVIS            PIC Z(5)9.                                   
003000*                                 AVISERAT ANTAL                          
003100*                                 QUANTITY NOTIFIED                       
003200     03 RR-KVANTMOT          PIC X(6).                                    
003300*                                 ANTAL MOTTAGET                          
003400*                                 QUANTITY RECEIVED                       
003500     03 RR-IDLBBET           PIC X(12).                                   
003600*                                 LASTBÄRARBETECKNING                     
003700*                                 TRAILER NUMBER                          
003800     03 RR-KDFARLIG          PIC X.                                       
003900*                                 KOD FÖR FARLIGT GODS                    
004000*                                 DANGEROUS GOODS CODE                    
004100     03 RR-KVQPACK-3         PIC Z(4)9.                                   
004200*                                 ANTAL I Q3 FÖRPACKNING                  
004300*                                 QUANTITY IN BULK PACK Q3                
004400     03 RR-KDSORT            PIC X(2).                                    
004500*                                 SORT-KOD                                
004600*                                 UNIT OF MEASURE                         
004700     03 RR-BEFT              PIC Z9.                                      
004800*                                 FÖRPACKNINGSTYP                         
004900*                                 PACKAGING TYPE                          
005000     03 RR-KVAVIS-PRIO       PIC Z(5)9.                                   
005100*                                 BERÄKN PRIORITERAD KVANT TOT            
005200*                                 CALC PRIO QUANTITY TOT                  
005300     03 RR-KVROS             PIC Z(6)9-.                                  
005400*                                 RESTORDERSALDO                          
005500*                                 BACKORDER QTY                           
005600     03 RR-ADLAGOMR          PIC Z9.                                      
005700*                                 LAGEROMRÅDE                             
005800*                                 AREA                                    
005900     03 RR-ADGANG            PIC Z9.                                      
006000*                                 GÅNG                                    
006100*                                 AISLE                                   
006200     03 RR-ADPLATS           PIC Z(4)9.                                   
006300*                                 LAGERPLATSNUMMER                        
006400*                                 LOCATION                                
006500     03 RR-BEARTURS          PIC X(15).                                   
006600*                                 ARTIKELURSPRUNGSLAND BENÄMNING          
006700*                                 COUNTRY OF ORIGIN DESCRIPTION           
006800     03 RR-KDKVAANT          PIC X.                                       
006900*                                 KOD ANTALSKONTR SKALL UTFÖRAS           
007000*                                 CODE THE QUANT WILL BE COUNTED          
007100     03 RR-KVKVAPRIM-BER     PIC Z(5)9.                                   
007200*                                 BER ANTAL TILL PRIMÄRKONTROLL           
007300*                                 CALC QTY TO PRIMARY CONTROL             
007400     03 RR-ADBUFF            OCCURS 3 TIMES.                              
007500*                                 BUFFER ADDRESS                          
007600        05 RR-ADBUFFGANG     PIC 9(2).                                    
007700*                                 BUFFERT GÅNG                            
007800        05 RR-ADBUFFOMR      PIC 9(2).                                    
007900*                                 BUFFERTOMRÅDE                           
008000*                                 BUFFER AREA                             
008100        05 RR-ADBUFFPL       PIC 9(5).                                    
008200*                                 BUFFERPLATSNUMMER                       
008300*                                 LOCATION IN BUFFER                      
008400     03 RR-VKART             PIC Z(6)9.                                   
008500*                                 ARTIKELVIKT (G)                         
008600*                                 PART WEIGHT (G)                         
008700     03 RR-VLARTNTO          PIC Z(7)9.9.                                 
008800*                                 ARTIKELVOLYM NETTO (CM3)                
008900*                                 PART NET VOLUME    (CM3)                
009000     03 RR-ADINLOMR-NXT1     PIC X(4).                                    
009100*                                 INLEVERANSOMRÅDE                        
009200*                                 RECEIVING AREA                          
009300     03 RR-IDANSK            PIC Z(2)9.                                   
009400*                                 ANSKAFFARNUMMER                         
009500*                                 PROCURER NO.                            
009600     03 RR-BELEV             PIC X(35).                                   
009700*                                 LEVERANTÖRSNAMN                         
009800*                                 SUPPLIER NAME                           
009900*** END OF VILMAII-COPY LENGTH= 236 BYTES                                 
