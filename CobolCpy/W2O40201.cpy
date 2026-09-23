000100 01  W2O40201.                                                            
000200*                                 COPYTEXT FÖR MOD W2O40201               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 ARTIKEL-UT.                                                       
001000        05 IDARTNR-UT        PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 STRECK-1          PIC X.                                       
001300        05 REKSIFFR          PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 IDDC-IN              PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 IDDC-UT              PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 BEART-ENG            PIC X(25).                                   
002000*                                 ENGELSK ARTIKELBENÄMNING                
002100     03 AREA-OUTPUT.                                                      
002200*                                                                         
002300        05 IDANSK            PIC Z9(2).                                   
002400*                                 ANSKAFFARNUMMER                         
002500        05 IDLEVNR-SHIP      PIC X(5).                                    
002600*                                 SKEPPANDE LEVERANTÖR                    
002700        05 KDAVT             PIC 9.                                       
002800*                                 AVTALSMÄRKNING                          
002900        05 KDANSKSEG         PIC 9(4).                                    
003000*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
003100        05 PRMATRL           PIC Z(6)9.9(2).                              
003200*                                 FAST PRIS UNDER LÖPANDE ÅR              
003300        05 KDMATRPR          PIC X.                                       
003400*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
003500        05 IDPLANGR-AG       PIC 9.                                       
003600*                                 PLANERINGSGRUPP ANSKAFFARE              
003700        05 IDLEVNR-AVT       PIC X(5).                                    
003800*                                 LEVERANTÖR ENLIGT AVTAL                 
003900        05 KDPRISKL          PIC X.                                       
004000*                                 PRISKLASS                               
004100        05 KDFREKKL          PIC X.                                       
004200*                                 FREKVENSKLASS                           
004300        05 IDREFTAB          PIC X.                                       
004400*                                 IDENTITET REFILLTABELL                  
004500        05 LAGERPLATS.                                                    
004600           07 ADLAGOMR       PIC Z9.                                      
004700*                                 LAGEROMRÅDE                             
004800           07 ADGANG         PIC Z(2)9.                                   
004900*                                 GÅNG                                    
005000           07 ADPLATS        PIC Z(5)9.                                   
005100*                                                     ADPLATS-002         
005200*                                 LAGERPLATS OMR. GÅNG PLATS              
005300        05 KOLUMN-1-2.                                                    
005400*                                                                         
005500           07 KVLS           OCCURS 2 TIMES                               
005600                             PIC -(7)9.                                   
005700*                                 LAGERSALDO                              
005800           07 KVRESS         OCCURS 2 TIMES                               
005900                             PIC -(7)9.                                   
006000*                                 RESERVERAT ANTAL ARTIKLAR               
006100           07 STONHND        OCCURS 2 TIMES                               
006200                             PIC -(7)9.                                   
006300*                                 DISPONIBELT LAGER                       
006400           07 KVOKS          OCCURS 2 TIMES                               
006500                             PIC -(7)9.                                   
006600*                                 ORDERKÖSALDO                            
006700           07 KVAKS-SDC      OCCURS 2 TIMES                               
006800                             PIC -(7)9.                                   
006900*                                 ANKOMSTSALDO                            
007000           07 KVAKS-PAV      OCCURS 2 TIMES                               
007100                             PIC -(7)9.                                   
007200*                                 DEL AV AK PÅ VÄG                        
007300           07 KVAVIS         PIC -(7)9.                                   
007400*                                 AVISERAT ANTAL                          
007500           07 KVROS          OCCURS 2 TIMES                               
007600                             PIC -(7)9.                                   
007700*                                 RESTORDERSALDO                          
007800           07 KVBEART-Q      PIC -(7)9.                                   
007900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
008000        05 KDERS             PIC X(2).                                    
008100*                                 ERSÄTTNINGSKOD                          
008200        05 FLTOVIPS          PIC X.                                       
008300        05 TIURPROD          PIC 9(4).                                    
008400*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
008500        05 KOLUMN-3-5.                                                    
008600*                                                                         
008700           07 TILEVPL        PIC 9(6).                                    
008800*                                 LEVERANSPLANEDATUM  (ÅÅMMDD)            
008900           07 TIREFEFT       PIC 9(6).                                    
009000*                                 DATUM SENAST EFTERFRÅGAD                
009100           07 KVPB-REF       OCCURS 2 TIMES                               
009200                             PIC Z(6)9.9.                                 
009300*                                 PERIODBEHOV REFILLING                   
009400           07 TIREFMPB       PIC 9(6).                                    
009500*                                 DATUM MANUELL PROGNOS REFILLING         
009600           07 KVSLAGER       PIC -(7)9.                                   
009700*                                 SÄKERHETSLAGER                          
009800           07 TIMANSEC       PIC 9(6).                                    
009900*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
010000           07 KVREFBER       PIC Z(6)9.                                   
010100*                                 BERÄKNAD REFILLINGKVANTITET             
010200           07 TIREFPAF       PIC 9(6).                                    
010300*                                 DATUM MANUELL PÅFYLLNADSKVANT           
010400           07 TIINVDAT       PIC 9(5).                                    
010500*                                 INVENTERINGSDATUM                       
010600           07 KVUTRS         OCCURS 2 TIMES                               
010700                             PIC -(7)9.                                   
010800*                                 UTREDNINGSSALDO                         
010900           07 KVEFRS         OCCURS 2 TIMES                               
011000                             PIC -(7)9.                                   
011100*                                 EJ FAKTURERAT ANTAL STYCK               
011200           07 KVSPANT        OCCURS 2 TIMES                               
011300                             PIC -(7)9.                                   
011400*                                 SPÄRRAT ANTAL                           
011500           07 KVREFOVL       PIC Z(6)9.                                   
011600*                                 LAGERSALDO                              
011700     03 TEMFSINF             PIC X(55).                                   
011800*                                 INFORMATIONSMEDDELANDE                  
011900*** END OF VILMAII-COPY LENGTH= 450 BYTES                                 
