000100 01  RR-W6019E1.                                                          
000200*                                 DATA FOR PRE TREATMENT REPORT           
000300*                                 W6019E-001                              
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
001500     03 RR-KVAVIS            PIC Z(5)9.                                   
001600*                                 AVISERAT ANTAL                          
001700*                                 QUANTITY NOTIFIED                       
001800     03 RR-KVQPACK-3         PIC Z(4)9.                                   
001900*                                 ANTAL I Q3 FÖRPACKNING                  
002000*                                 QUANTITY IN BULK PACK Q3                
002100     03 RR-KDSORT            PIC X(2).                                    
002200*                                 SORT-KOD                                
002300*                                 UNIT OF MEASURE                         
002400     03 RR-BEFT              PIC Z9.                                      
002500*                                 FÖRPACKNINGSTYP                         
002600*                                 PACKAGING TYPE                          
002700     03 RR-KVAVIS-PRIO       PIC Z(5)9.                                   
002800*                                 BERÄKN PRIORITERAD KVANT TOT            
002900*                                 CALC PRIO QUANTITY TOT                  
003000     03 RR-KVROS             PIC Z(6)9-.                                  
003100*                                 RESTORDERSALDO                          
003200*                                 BACKORDER QTY                           
003300     03 RR-ADLAGOMR          PIC Z9.                                      
003400*                                 LAGEROMRÅDE                             
003500*                                 AREA                                    
003600     03 RR-ADGANG            PIC Z9.                                      
003700*                                 GÅNG                                    
003800*                                 AISLE                                   
003900     03 RR-ADPLATS           PIC Z(4)9.                                   
004000*                                 LAGERPLATSNUMMER                        
004100*                                 LOCATION                                
004200     03 RR-BEARTURS          PIC X(15).                                   
004300*                                 ARTIKELURSPRUNGSLAND BENÄMNING          
004400*                                 COUNTRY OF ORIGIN DESCRIPTION           
004500     03 RR-KDKVAANT          PIC X.                                       
004600*                                 KOD ANTALSKONTR SKALL UTFÖRAS           
004700*                                 CODE THE QUANT WILL BE COUNTED          
004800     03 RR-KVKVAPRIM-BER     PIC Z(5)9.                                   
004900*                                 BER ANTAL TILL PRIMÄRKONTROLL           
005000*                                 CALC QTY TO PRIMARY CONTROL             
005100     03 RR-VKART             PIC Z(6)9.                                   
005200*                                 ARTIKELVIKT (G)                         
005300*                                 PART WEIGHT (G)                         
005400     03 RR-VLARTNTO          PIC Z(7)9.9.                                 
005500*                                 ARTIKELVOLYM NETTO (CM3)                
005600*                                 PART NET VOLUME    (CM3)                
005700     03 RR-FP                OCCURS 3 TIMES.                              
005800*                                 BUFFER ADDRESS                          
005900        05 RR-FP-ADLAGOMR    PIC Z9.                                      
006000*                                 LAGEROMRÅDE                             
006100*                                 AREA                                    
006200        05 RR-FP-ADGANG      PIC Z9.                                      
006300*                                 GÅNG                                    
006400*                                 AISLE                                   
006500        05 RR-FP-ADPLATS     PIC Z(4)9.                                   
006600*                                 LAGERPLATSNUMMER                        
006700*                                 LOCATION                                
006800        05 RR-FP-IDARTNR     PIC Z(7)9.                                   
006900*                                 ARTIKELNUMMER                           
007000*                                 PART NUMBER                             
007100        05 RR-FP-BEART       PIC X(25).                                   
007200*                                 ARTIKELBENÄMNING                        
007300*                                 PART DESCRIPTION                        
007400        05 RR-FP-KDEMBKOD    PIC Z(2)9.                                   
007500*                                 EMBALLAGEKOD                            
007600        05 RR-FP-KVQPACK     PIC Z(4)9.                                   
007700*                                 ANTAL KVANTITETFÖRPACKNINGAR            
007800*                                 NO OF BULK PACKS                        
007900*** END OF VILMAII-COPY LENGTH= 270 BYTES                                 
