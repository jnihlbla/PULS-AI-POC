000100 01  W23192-CTX.                                                          
000200*                                 COPYTEXT TILL FIL W23192/W23193         
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001000*                                 PRODUKTSLAG                             
001100     03 KDPSLLOC             PIC 9(2).                                    
001200*                                 PRODUKTSLAG LOKALT                      
001300     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001400*                                 PERSONKOD REFILLANSVARIG                
001500     03 KDFREKKL             PIC X.                                       
001600*                                 FREKVENSKLASS                           
001700     03 KDPRISKL             PIC X.                                       
001800*                                 PRISKLASS                               
001900     03 KDREFSTA             PIC X.                                       
002000*                                 STATUS REFILLARTIKEL                    
002100     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
002200*                                 DEL AV AK SOM LIGGER I SDC              
002300     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002400*                                 DEL AV AK PÅ VÄG                        
002500     03 KVLS                 PIC S9(7)           COMP-3.                  
002600*                                 LAGERSALDO                              
002700     03 KVOKS                PIC S9(7)           COMP-3.                  
002800*                                 ORDERKÖSALDO                            
002900     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
003000*                                 PERIODBEHOV REFILLING                   
003100     03 KVREFBER             PIC S9(7)           COMP-3.                  
003200*                                 BERÄKNAD REFILLINGKVANTITET             
003300     03 KVREFOVL             PIC S9(7)           COMP-3.                  
003400*                                 BERÄKNAD ÖVERLAGERPUNKT                 
003500     03 KVREFPKT             PIC S9(7)           COMP-3.                  
003600*                                 BERÄKNAD PÅFYLLNADSPUNKT                
003700     03 KVRESS               PIC S9(7)           COMP-3.                  
003800*                                 RESERVERAT ANTAL ARTIKLAR               
003900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 ARTIKELSTANDARDPRIS                     
004100     03 W23192-001-GRP       OCCURS 53 TIMES.                             
004200*                                 RULLANDE FÖRBRUKNING                    
004300        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
004400*                                 ORDERTRÄFFAR PÅ SDC                     
004500        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
004600*                                 ORDERTRÄFFAR LEV FRÅN CDC               
004700     03 W23192-002-GRP       OCCURS 5 TIMES.                              
004800*                                 INNEVARANDE FÖRBRUKNING                 
004900        05 TIVV              PIC S9(3)           COMP-3.                  
005000*                                 VECKA  (VV)                             
005100        05 KVOT-INNEV        PIC S9(7)           COMP-3.                  
005200*                                 ORDERTRÄFFAR PÅ SDC                     
005300        05 KVOT-CDC-INNEV    PIC S9(7)           COMP-3.                  
005400*                                 ORDERTRÄFFAR LEV FRÅN CDC               
005500     03 W23192-003-GRP       OCCURS 53 TIMES.                             
005600*                                 RULLANDE FÖRBRUKNING                    
005700        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
005800*                                 ORDERINGÅNG TILL SDC                    
005900     03 IDREFTAB             PIC X.                                       
006000*                                 IDENTITET REFILLTABELL                  
006100     03 PRMATRL              PIC S9(7)V9(2)      COMP-3.                  
006200*                                 FAST PRIS UNDER LÖPANDE ÅR              
006300*** END OF VILMAII-COPY LENGTH= 754 BYTES                                 
