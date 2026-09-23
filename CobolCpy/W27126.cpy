000100 01  W27126.                                                              
000200*                                 COPYTEXT TILL FILEN W27126              
000300*                                 SKAPA EXTRAKTFIL FÖR                    
000400*                                 UTSÖKNINGAR FRÅN 2348                   
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDDC-REF             PIC X(2).                                    
001000*                                 SÄNDANDE LAGER FÖR REFILL               
001100     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001200*                                 PERSONKOD REFILLANSVARIG                
001300     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
001400*                                 PERIODBEHOV REFILLING                   
001500     03 ADART.                                                            
001600*                                 ARTIKELADRESS I LAGRET                  
001700        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE                             
001900        05 ADGANG            PIC S9(3)           COMP-3.                  
002000*                                 GÅNG                                    
002100        05 ADPLATS           PIC S9(5)           COMP-3.                  
002200*                                 LAGERPLATSNUMMER                        
002300     03 KVLS                 PIC S9(7)           COMP-3.                  
002400*                                 LAGERSALDO                              
002500     03 IDPROJ               PIC X(4).                                    
002600*                                 PARTS PROJEKTIDENTITET                  
002700     03 KDERS                PIC S9(3)           COMP-3.                  
002800*                                 ERSÄTTNINGSKOD                          
002900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
003000*                                 PRODUKTSLAG                             
003100     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003200*                                 FUNKTIONSGRUPP                          
003300     03 KLASS                PIC X(3).                                    
003400     03 TIREFEFT             PIC S9(7)           COMP-3.                  
003500*                                 DATUM SENAST EFTERFRÅGAD                
003600     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
003700*                                 BESTÄLLNINGSPRIS I KRONOR               
003800     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003900*                                 ARTIKELSTANDARDPRIS                     
004000     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
004100*                                 DEL AV AK PÅ VÄG                        
004200     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
004300*                                 DEL AV AK SOM LIGGER I SDC              
004400     03 KVBEART              PIC S9(7)           COMP-3.                  
004500*                                 BESTÄLLT ANTAL STYCKEN                  
004600     03 KVREFBER             PIC S9(7)           COMP-3.                  
004700*                                 BERÄKNAD REFILLINGKVANTITET             
004800     03 KVREFPKT             PIC S9(7)           COMP-3.                  
004900*                                 BERÄKNAD PÅFYLLNADSPUNKT                
005000     03 KVROS-DAG            PIC S9(7)           COMP-3.                  
005100*                                 RESTORDERSALDO, KLASS 1                 
005200     03 KVROS-BULK           PIC S9(7)           COMP-3.                  
005300*                                 RESTORDERSALDO, KLASS 2-4               
005400     03 TIFINLV              PIC S9(5)           COMP-3.                  
005500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
005600     03 TIREFPAF             PIC S9(7)           COMP-3.                  
005700*                                 DATUM MANUELL PÅFYLLNADSKVANT           
005800     03 TIREFPKT             PIC S9(7)           COMP-3.                  
005900*                                 DATUM MANUELL REFILLPUNKT               
006000     03 IDLEVNR-CDC          PIC X(5).                                    
006100*                                 LEVERANTÖRNUMMER                        
006200     03 IDLEVNR-DC           PIC X(5).                                    
006300*                                 LEVERANTÖRNUMMER                        
006400     03 FLWILSON             PIC X.                                       
006500*                                 WILSONFORMEL                            
006600     03 FLREFILL             PIC X.                                       
006700*                                 REFILLARTIKEL                           
006800     03 FLREFBEO             PIC X.                                       
006900*                                 AUTOMATISK REFILL BEORDRING?            
007000     03 IDREFTAB             PIC X.                                       
007100*                                 IDENTITET REFILLTABELL                  
007200     03 FLFLYG               PIC X.                                       
007300*                                 FLYGARTIKEL                             
007400     03 KVDAGAR-MANLT        PIC S9(3)           COMP-3.                  
007500*                                 ANTAL DAGAR                             
007600     03 VKART                PIC S9(7)           COMP-3.                  
007700*                                 ARTIKELVIKT (G)                         
007800     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
007900*                                 ARTIKELVOLYM NETTO (CM3)                
008000     03 RESEASON             OCCURS 12 TIMES                              
008100                             PIC S9V9(2)         COMP-3.                  
008200*                                 SÄSONGSINDEX                            
008300     03 BEART                PIC X(25).                                   
008400*                                 ARTIKELBENÄMNING                        
008500     03 BEART-ENG            PIC X(25).                                   
008600*                                 ENGELSK ARTIKELBENÄMNING                
008700     03 KDREFSTA             PIC X.                                       
008800*                                 STATUS REFILLARTIKEL                    
008900     03 SUPERWEEK            PIC S9(3)           COMP-3.                  
009000     03 BEMODELL             OCCURS 45 TIMES                              
009100                             PIC X(15).                                   
009200*                                 BILENS MODELLBESKRIVNING.               
009300     03 KVPBREOI             PIC S9(6)V9(1)      COMP-3.                  
009400*                                 PERIODBEHOV FÖR REFILL OI               
009500*** END OF VILMAII-COPY LENGTH= 875 BYTES                                 
