000100 01  PUNK-W224PUNK.                                                       
000200*                                                                         
000300     03 PUNK-INDATA.                                                      
000400        05 PUNK-IDARTNR      PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600        05 PUNK-IDDC         PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800        05 PUNK-IDDC-REF     PIC X(2).                                    
000900*                                 SÄNDANDE LAGER FÖR REFILL               
001000        05 PUNK-TIAAVV-AKT   PIC S9(5)           COMP-3.                  
001100*                                 ÅR - VECKA  (ÅÅVV)                      
001200        05 PUNK-TIAAVVD-AKT  PIC S9(5)           COMP-3.                  
001300*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001400        05 PUNK-IDREFTAB     PIC X.                                       
001500*                                 IDENTITET REFILLTABELL                  
001600        05 PUNK-FLREFBEO     PIC X.                                       
001700*                                 AUTOMATISK REFILL BEORDRING?            
001800        05 PUNK-FLWILSON     PIC X.                                       
001900*                                 WILSONFORMEL                            
002000        05 PUNK-KVREFPKT-IN  PIC S9(7)           COMP-3.                  
002100*                                 BERÄKNAD PÅFYLLNADSPUNKT                
002200        05 PUNK-TIREFPKT-IN  PIC S9(7)           COMP-3.                  
002300*                                 DATUM MANUELL REFILLPUNKT               
002400        05 PUNK-KVREFBER-IN  PIC S9(7)           COMP-3.                  
002500*                                 BERÄKNAD REFILLINGKVANTITET             
002600        05 PUNK-TIREFPAF-IN  PIC S9(7)           COMP-3.                  
002700*                                 DATUM MANUELL PÅFYLLNADSKVANT           
002800        05 PUNK-KVSLAGER-IN  PIC S9(7)           COMP-3.                  
002900*                                 SÄKERHETSLAGER                          
003000        05 PUNK-TIMANSEC-IN  PIC S9(7)           COMP-3.                  
003100*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
003200        05 PUNK-RESEASON     OCCURS 12 TIMES                              
003300                             PIC S9V9(2)         COMP-3.                  
003400*                                 SÄSONGSINDEX                            
003500        05 PUNK-FLFLYG       PIC X.                                       
003600*                                 FLYGARTIKEL                             
003700        05 PUNK-IDLEVNR      PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900        05 PUNK-KVPALL       PIC S9(7)           COMP-3.                  
004000*                                 ANTAL I PALL                            
004100        05 PUNK-KVULOAD      PIC S9(7)           COMP-3.                  
004200*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
004300        05 PUNK-KVPB-REF     PIC S9(6)V9(1)      COMP-3.                  
004400*                                 PERIODBEHOV REFILLING                   
004500        05 PUNK-KVPBREOI     PIC S9(6)V9(1)      COMP-3.                  
004600*                                 PERIODBEHOV FÖR REFILL OI               
004700        05 PUNK-PRMATRL      PIC S9(7)V9(2)      COMP-3.                  
004800*                                 FAST PRIS UNDER LÖPANDE ÅR              
004900        05 PUNK-PRARTBES     PIC S9(7)V9(2)      COMP-3.                  
005000*                                 BESTÄLLNINGSPRIS I KRONOR               
005100        05 PUNK-ADLAGOMR     PIC S9(3)           COMP-3.                  
005200*                                 LAGEROMRÅDE                             
005300     03 PUNK-UTDATA.                                                      
005400        05 PUNK-KVREFBER     PIC S9(7)           COMP-3.                  
005500*                                 BERÄKNAD REFILLINGKVANTITET             
005600        05 PUNK-KVREFPKT     PIC S9(7)           COMP-3.                  
005700*                                 BERÄKNAD PÅFYLLNADSPUNKT                
005800        05 PUNK-KVREFOVL     PIC S9(7)           COMP-3.                  
005900*                                 BERÄKNAD ÖVERLAGERPUNKT                 
006000        05 PUNK-KVSLAGER     PIC S9(7)           COMP-3.                  
006100*                                 SÄKERHETSLAGER                          
006200        05 PUNK-KVEOQ        PIC S9(7)           COMP-3.                  
006300*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
006400*                                 ET                                      
006500        05 PUNK-TIREFPAF     PIC S9(7)           COMP-3.                  
006600*                                 DATUM MANUELL PÅFYLLNADSKVANT           
006700        05 PUNK-TIMANSEC     PIC S9(7)           COMP-3.                  
006800*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
006900        05 PUNK-TIREFPKT     PIC S9(7)           COMP-3.                  
007000*                                 DATUM MANUELL REFILLPUNKT               
007100*** END OF VILMAII-COPY LENGTH= 132 BYTES                                 
