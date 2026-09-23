000100 01  W4O10301.                                                            
000200*                                 COPYTEXT FÖR MOD W4O10301               
000300*                                                                         
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 KVERS                PIC 9(3).                                    
000900*                                 ANTAL ERS SOM LÄSES FÖRBI               
001000     03 IDARTNR-IN           PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 IDARTNR-UT           PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 STRECK               PIC X.                                       
001500     03 REKSIFFR             PIC 9.                                       
001600*                                 KONTROLLSIFFRA                          
001700     03 AREA.                                                             
001800*                                                                         
001900        05 BEART-SVE         PIC X(15).                                   
002000*                                 SVENSK ARTIKELBENÄMNING                 
002100        05 BEART-ENG         PIC X(15).                                   
002200*                                 ENGELSK           BEART-ENG-002         
002300*                                 ARTIKELBENÄMNING                        
002400        05 BEART-FRA         PIC X(15).                                   
002500*                                 FRANSK BENÄMNING  BEART-FRA-002         
002600        05 BEART-SPA         PIC X(15).                                   
002700*                                 SPAVSK BENÄMNING  BEART-SPA-002         
002800        05 BEART-TYS         PIC X(15).                                   
002900*                                 TYSK BENÄMNING    BEART-TYS-002         
003000        05 KVDISP            OCCURS 2 TIMES                               
003100                             PIC -(7)9.                                   
003200*                                 DISPONIBELT LAGER                       
003300        05 KVRESS            PIC -(7)9.                                   
003400*                                 RESERVERAT ANTAL ARTIKLAR               
003500        05 KVUTRS            OCCURS 2 TIMES                               
003600                             PIC -(7)9.                                   
003700*                                 UTREDNINGSSALDO                         
003800        05 KVSLAGER          PIC -(7)9.                                   
003900*                                 SÄKERHETSLAGER                          
004000        05 KVAKS             OCCURS 2 TIMES                               
004100                             PIC -(7)9.                                   
004200*                                 ANKOMSTSALDO                            
004300        05 IDAVINR           PIC Z(6)9.                                   
004400*                                 AVI-NUMMER                              
004500        05 KVROS             PIC -(7)9.                                   
004600*                                 RESTORDERSALDO                          
004700        05 KVPB              OCCURS 2 TIMES                               
004800                             PIC -(7)9.                                   
004900*                                 PERIODBEHOV            KVPB-002         
005000        05 KVSPANT           PIC -(7)9.                                   
005100*                                 SPÄRRAT ANTAL                           
005200        05 IDANSK            PIC Z9(2).                                   
005300*                                 ANSKAFFARNUMMER                         
005400        05 IDLEVNR           PIC X(5).                                    
005500*                                 LEVERANTÖRNUMMER                        
005600        05 KDHF              PIC 9.                                       
005700*                                 HUVUDFÖRRÅDSMÄRKNING                    
005800        05 SPARRKOD          PIC X.                                       
005900*                                 SPÄRRKOD                                
006000        05 FILLER.                                                        
006100           07 ADLAGOMR       PIC Z9B.                                     
006200*                                 LAGEROMRÅDE                             
006300           07 ADGANG         PIC Z9B.                                     
006400*                                 GÅNG                                    
006500           07 ADPLATS        PIC Z(4)9.                                   
006600*                                 LAGERPLATSNUMMER                        
006700        05 FLERPL            PIC X.                                       
006800        05 KVQPACK-1         PIC Z(3)9.                                   
006900*                                 ANTAL I Q1 FÖRPACKNING                  
007000        05 FIKT-QPACK        PIC Z(2)9.                                   
007100*                                 FIKTIV KVANT                            
007200        05 REDIRLEV          PIC 9.9(2).                                  
007300*                                 DIREKTLEVERANSANDEL                     
007400        05 KDERS             PIC Z9(2).                                   
007500*                                 ERSÄTTNINGSKOD                          
007600        05 TIERSDAT          PIC Z(4)9.                                   
007700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007800        05 FILLER.                                                        
007900           07 IDPSN-TEXT     PIC X(10).                                   
008000           07 IDPSN          PIC Z(2)9.                                   
008100*                                 PROPER SHIPPING NAME                    
008200        05 KDFARLIG-ATTR     PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 FILLER.                                                        
008500           07 KDFARLIG-TEXT  PIC X(12).                                   
008600           07 KDFARLIG       PIC 9.                                       
008700*                                 KOD FÖR FARLIGT GODS                    
008800        05 KDSRA             PIC Z(2)9.                                   
008900*                                 SRA-KOD                                 
009000        05 KDARTURS          PIC X(2).                                    
009100*                                 ARTIKELURSPRUNGSKOD                     
009200        05 ERS-TEXT          PIC X(20).                                   
009300*                                 ERSÄTTNINGSTEXT                         
009400        05 TYP1.                                                          
009500           07 FILLER         OCCURS 10 TIMES.                             
009600              09 IDARTNR-TILLK                                            
009700                             PIC Z(8)9.                                   
009800*                                 TILLKOMMANDE ARTIKELNUMMER              
009900              09 FILLER      PIC X(2).                                    
010000              09 DIERS-TILLK PIC Z(3)9.9(3).                              
010100*                                 TILLKOMMANDE ARTIKELANTAL               
010200              09 FILLER      PIC X.                                       
010300        05 TYP2 REDEFINES TYP1.                                           
010400           07 FILLER         OCCURS 10 TIMES.                             
010500              09 BEERS       PIC X(20).                                   
010600*                                 ERSÄTTNINGSTEXT                         
010700        05 FLER-RADER        PIC X(20).                                   
010800     03 FLPCOO               PIC X.                                       
010900*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
011000     03 TEMFSINF             PIC X(55).                                   
011100*                                 INFORMATIONSMEDDELANDE                  
011200*** END OF VILMAII-COPY LENGTH= 615 BYTES                                 
