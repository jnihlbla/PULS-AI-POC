000100 01  W231701A-CTX.                                                        
000200*                                 COPYTEXT TILL FIL W23170                
000300     03 IDDC                 PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 IDDC-REF             PIC X(2).                                    
000600*                                 LAGER FÖR REFILL                        
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KDFREKKL             PIC X.                                       
001000*                                 FREKVENSKLASS                           
001100     03 KDPRISKL             PIC X.                                       
001200*                                 PRISKLASS                               
001300     03 KDREFSTA             PIC X.                                       
001400*                                 STATUS REFILLARTIKEL                    
001500     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
001600*                                 DEL AV AK SOM LIGGER I SDC              
001700     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
001800*                                 DEL AV AK PÅ VÄG                        
001900     03 KVLS                 PIC S9(7)           COMP-3.                  
002000*                                 LAGERSALDO                              
002100     03 KVOKS                PIC S9(7)           COMP-3.                  
002200*                                 ORDERKÖSALDO                            
002300     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
002400*                                 PERIODBEHOV REFILLING                   
002500     03 KVREFBER             PIC S9(7)           COMP-3.                  
002600*                                 BERÄKNAD REFILLINGKVANTITET             
002700     03 KVREFOVL             PIC S9(7)           COMP-3.                  
002800*                                 BERÄKNAD ÖVERLAGERPUNKT                 
002900     03 KVREFPKT             PIC S9(7)           COMP-3.                  
003000*                                 BERÄKNAD PÅFYLLNADSPUNKT                
003100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELSTANDARDPRIS                     
003300     03 W231701A-001-GRP     OCCURS 53 TIMES.                             
003400*                                 RULLANDE FÖRBRUKNING                    
003500        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
003600*                                 ORDERTRÄFFAR PÅ SDC                     
003700        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
003800*                                 ORDERTRÄFFAR LEV FRÅN CDC               
003900     03 W231701A-002-GRP     OCCURS 5 TIMES.                              
004000*                                 INNEVARANDE FÖRBRUKNING                 
004100        05 TIVV              PIC S9(3)           COMP-3.                  
004200*                                 VECKA  (VV)                             
004300        05 KVOT-INNEV        PIC S9(7)           COMP-3.                  
004400*                                 ORDERTRÄFFAR PÅ SDC                     
004500        05 KVOT-CDC-INNEV    PIC S9(7)           COMP-3.                  
004600*                                 ORDERTRÄFFAR LEV FRÅN CDC               
004700     03 W231701A-003-GRP     OCCURS 53 TIMES.                             
004800*                                 RULLANDE FÖRBRUKNING                    
004900        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
005000*                                 ORDERINGÅNG TILL SDC                    
005100     03 IDREFTAB             PIC X.                                       
005200*                                 IDENTITET REFILLTABELL                  
005300*** END OF VILMAII-COPY LENGTH= 736 BYTES                                 
