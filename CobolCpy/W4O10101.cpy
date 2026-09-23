000100 01  W4O10101.                                                            
000200*                                 COPYTEXT FÖR MOD W4O10101               
000300*                                                                         
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 VAGNSKIP             PIC 9(3).                                    
000900     03 IDARTNR-IN           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 STRECK               PIC X.                                       
001400     03 AREA.                                                             
001500*                                 AREA SOM NOLLSTÄLLS MELLAN              
001600*                                 VARVEN                                  
001700*                                                                         
001800        05 REKSIFFR          PIC 9.                                       
001900*                                 KONTROLLSIFFRA                          
002000        05 BEART-SVE         PIC X(25).                                   
002100*                                 SVENSK ARTIKELBENÄMNING                 
002200        05 ADARTADR.                                                      
002300*                                 ADRESS I LAGER                          
002400*                                                                         
002500           07 ADLAGOMR       PIC Z9B.                                     
002600*                                 LAGEROMRÅDE                             
002700           07 ADGANG         PIC Z9B.                                     
002800*                                 GÅNG                                    
002900           07 ADPLATS        PIC Z(4)9.                                   
003000*                                 LAGERPLATSNUMMER                        
003100        05 FLERPL            PIC X.                                       
003200        05 KVLS              OCCURS 2 TIMES                               
003300                             PIC -(7)9.                                   
003400*                                 LAGERSALDO                              
003500        05 KVAKS             OCCURS 2 TIMES                               
003600                             PIC -(7)9.                                   
003700*                                 ANKOMSTSALDO                            
003800        05 KVAKS-PAV         OCCURS 2 TIMES                               
003900                             PIC -(7)9.                                   
004000*                                 DEL AV AK PÅ VÄG                        
004100        05 KVEFRS            OCCURS 2 TIMES                               
004200                             PIC -(7)9.                                   
004300*                                 EJ FAKTURERAT ANTAL STYCK               
004400        05 VKART             PIC Z(6)9.                                   
004500*                                 ARTIKELVIKT (G)                         
004600        05 VLARTNTO          PIC Z(7)9.9.                                 
004700*                                 ARTIKELVOLYM NETTO (CM3)                
004800        05 KDVSOP            PIC X(3).                                    
004900*                                 VSOP-KOD                                
005000        05 IDLEVNR           PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200        05 IDANSK            PIC Z9(2).                                   
005300*                                 ANSKAFFARNUMMER                         
005400        05 KDARTURS          OCCURS 2 TIMES                               
005500                             PIC X(2).                                    
005600*                                 ARTIKELURSPRUNGSKOD                     
005700        05 KDARTHNT          PIC Z(5)9(3).                                
005800*                                 HANTERINGSKOD      KDARTHNT-002         
005900        05 KDGK              PIC 9.                                       
006000*                                 GODSMOTTAGAREKOD                        
006100        05 BEFT              OCCURS 2 TIMES                               
006200                             PIC Z(2)9.                                   
006300*                                 FÖRPACKNINGSTYP                         
006400        05 KDFORP            OCCURS 2 TIMES.                              
006500*                                 FÖRPACKNINGSKOD                         
006600           07 KDFORPPL       PIC 9.                                       
006700*                                 FÖRPACKNINGSPLATS                       
006800           07 KDFORPGP       PIC 9(2).                                    
006900*                                 FÖRPACKNINGSGRUPP                       
007000           07 KDFORPUF       PIC 9.                                       
007100*                                 UPPRÄKNINGSFAKTOR                       
007200        05 FLAGGA-KVAL-INFO  PIC X.                                       
007300*                                 ALLMÄN FLAGGA                           
007400        05 KVMP              PIC Z(7)9.                                   
007500*                                 MAXPUNKT               KVMP-002         
007600        05 KDERS             PIC Z9(2).                                   
007700*                                 ERSÄTTNINGSKOD                          
007800        05 KVPB-TOT          OCCURS 2 TIMES                               
007900                             PIC Z(6)9.9.                                 
008000*                                 PERIODBEHOV (PROGNOS)                   
008100        05 KVPB-SATS         PIC Z(6)9.9.                                 
008200*                                 SATS-PERIODBEHOV                        
008300        05 IDARTNR-EMBQ0     OCCURS 2 TIMES                               
008400                             PIC Z(8)9.                                   
008500*                                 EMBALLAGEARTIKELNR FÖR Q0               
008600        05 KDEMBKOD-0        OCCURS 2 TIMES                               
008700                             PIC Z(2)9.                                   
008800*                                 EMBALLAGEKOD 0                          
008900        05 KVQPACK-0         PIC Z(4)9.                                   
009000*                                 ANTAL I Q0 FÖRPACKNING                  
009100        05 IDARTNR-EMBQ1     OCCURS 2 TIMES                               
009200                             PIC Z(8)9.                                   
009300*                                 EMBALLAGEARTIKELNR FÖR Q1               
009400        05 KDEMBKOD-1        OCCURS 2 TIMES                               
009500                             PIC Z(2)9.                                   
009600*                                 EMBALLAGEKOD 1                          
009700        05 KVQPACK-1         PIC Z(4)9.                                   
009800*                                 ANTAL I Q1 FÖRPACKNING                  
009900        05 IDARTNR-EMBQ2     OCCURS 2 TIMES                               
010000                             PIC Z(8)9.                                   
010100*                                 EMBALLAGEARTIKELNR FÖR Q2               
010200        05 KDEMBKOD-2        OCCURS 2 TIMES                               
010300                             PIC Z(2)9.                                   
010400*                                 EMBALLAGEKOD 2                          
010500        05 KVQPACK-2         PIC Z(4)9.                                   
010600*                                 ANTAL I Q2 FÖRPACKNING                  
010700        05 IDARTNR-EMBQ3     OCCURS 2 TIMES                               
010800                             PIC Z(8)9.                                   
010900*                                 EMBALLAGEARTIKELNR FÖR Q3               
011000        05 KVQPACK-3         PIC Z(4)9.                                   
011100*                                 ANTAL I Q3 FÖRPACKNING                  
011200        05 IDARTNR-EMBQ4     OCCURS 2 TIMES                               
011300                             PIC Z(8)9.                                   
011400*                                 EMBALLAGEARTIKELNR FÖR Q4               
011500        05 KVQPACK-4         PIC Z(4)9.                                   
011600*                                 ANTAL I Q4 FÖRPACKNING                  
011700        05 IDKAT             OCCURS 3 TIMES                               
011800                             PIC X(6).                                    
011900*                                 KATALOGTILLHÖRIGHET   IDKAT-002         
012000     03 FLPCOO               PIC X.                                       
012100*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
012200     03 TEMFSINF             PIC X(55).                                   
012300*                                 INFORMATIONSMEDDELANDE                  
012400*** END OF VILMAII-COPY LENGTH= 469 BYTES                                 
