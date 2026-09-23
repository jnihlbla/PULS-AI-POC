000100 01  W4O10201.                                                            
000200*                                 COPYTEXT FÖR MOD W4O10201               
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
001500*                                                                         
001600        05 REKSIFFR          PIC 9.                                       
001700*                                 KONTROLLSIFFRA                          
001800        05 BEART-SVE         PIC X(25).                                   
001900*                                 SVENSK ARTIKELBENÄMNING                 
002000        05 BEART-ENG         PIC X(25).                                   
002100*                                 ENGELSK ARTIKELBENÄMNING                
002200        05 IDLEVNR           PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 IDANSK            PIC Z9(2).                                   
002500*                                 ANSKAFFARNUMMER                         
002600        05 KDGK              PIC 9.                                       
002700*                                 GODSMOTTAGAREKOD                        
002800        05 VKART             PIC Z(6)9.                                   
002900*                                 ARTIKELVIKT (G)                         
003000        05 VLARTNTO          PIC Z(7)9.9.                                 
003100*                                 ARTIKELVOLYM NETTO (CM3)                
003200        05 KDSORT            PIC X(2).                                    
003300*                                 SORT-KOD                                
003400        05 IDFKNGRP          PIC Z(4)9.                                   
003500*                                 FUNKTIONSGRUPP                          
003600        05 KDVVKL            PIC 9.                                       
003700*                                 VOLYMVÄRDESKLASS                        
003800        05 PRINK             PIC Z(6)9.9(2).                              
003900*                                 INKÖPSPRIS                              
004000        05 PRARTBES          PIC Z(6)9.9(2).                              
004100*                                 BESTÄLLNINGSPRIS I KRONOR               
004200        05 PRARTSTD          PIC Z(6)9.9(2).                              
004300*                                 ARTIKELSTANDARDPRIS                     
004400        05 PRARTSJK          PIC Z(6)9.9(2).                              
004500*                                 ARTIKELNS SJÄLVKOSTNAD                  
004600        05 PRDIRLON          PIC Z(3)9.9(3).                              
004700*                                 DIREKT LÖN                              
004800        05 PRDMTRL           PIC Z(5)9.9(3).                              
004900*                                 DIREKT MATERIAL                         
005000        05 PROVRPAL          PIC Z(3)9.9(3).                              
005100*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
005200        05 ADARTADR.                                                      
005300*                                 ADRESS I LAGER                          
005400*                                                                         
005500           07 ADLAGOMR       PIC Z9B.                                     
005600*                                 LAGEROMRÅDE                             
005700           07 ADGANG         PIC Z9B.                                     
005800*                                 GÅNG                                    
005900           07 ADPLATS        PIC Z(4)9.                                   
006000*                                 LAGERPLATSNUMMER                        
006100        05 FLERPL            PIC X.                                       
006200        05 BEFT              OCCURS 2 TIMES                               
006300                             PIC Z(2)9.                                   
006400*                                 FÖRPACKNINGSTYP                         
006500        05 KDARTHNT          PIC Z(5)9(3).                                
006600*                                 HANTERINGSKOD      KDARTHNT-002         
006700        05 KDERS             PIC Z9(2).                                   
006800*                                 ERSÄTTNINGSKOD                          
006900        05 KVPB-TOT          OCCURS 2 TIMES                               
007000                             PIC Z(5)9.9.                                 
007100*                                 PERIODBEHOV (PROGNOS)                   
007200        05 KVPB-SATS         PIC Z(5)9.9.                                 
007300*                                 SATS-PERIODBEHOV                        
007400        05 KVAKS             OCCURS 2 TIMES                               
007500                             PIC -(7)9.                                   
007600*                                 ANKOMSTSALDO                            
007700        05 KVLS              OCCURS 2 TIMES                               
007800                             PIC -(7)9.                                   
007900*                                 LAGERSALDO                              
008000        05 KVBR              PIC -(7)9.                                   
008100*                                 BESTÄLLNINGSREST                        
008200        05 KDFARLIG          PIC 9.                                       
008300*                                 KOD FÖR FARLIGT GODS                    
008400        05 KDARTURS          OCCURS 2 TIMES                               
008500                             PIC X(2).                                    
008600*                                 ARTIKELURSPRUNGSKOD                     
008700        05 IDARTNR-EMBQ0     OCCURS 2 TIMES                               
008800                             PIC Z(8)9.                                   
008900*                                 EMBALLAGEARTIKELNR FÖR Q0               
009000        05 KDEMBKOD-0        OCCURS 2 TIMES                               
009100                             PIC Z(2)9.                                   
009200*                                 EMBALLAGEKOD 0                          
009300        05 KVQPACK-0         PIC Z(4)9.                                   
009400*                                 ANTAL I Q0 FÖRPACKNING                  
009500        05 IDARTNR-EMBQ1     OCCURS 2 TIMES                               
009600                             PIC Z(8)9.                                   
009700*                                 EMBALLAGEARTIKELNR FÖR Q1               
009800        05 KDEMBKOD-1        OCCURS 2 TIMES                               
009900                             PIC Z(2)9.                                   
010000*                                 EMBALLAGEKOD 1                          
010100        05 KVQPACK-1         PIC Z(4)9.                                   
010200*                                 ANTAL I Q1 FÖRPACKNING                  
010300        05 IDARTNR-EMBQ2     OCCURS 2 TIMES                               
010400                             PIC Z(8)9.                                   
010500*                                 EMBALLAGEARTIKELNR FÖR Q2               
010600        05 KDEMBKOD-2        OCCURS 2 TIMES                               
010700                             PIC Z(2)9.                                   
010800*                                 EMBALLAGEKOD 2                          
010900        05 KVQPACK-2         PIC Z(4)9.                                   
011000*                                 ANTAL I Q2 FÖRPACKNING                  
011100        05 IDARTNR-EMBQ3     OCCURS 2 TIMES                               
011200                             PIC Z(8)9.                                   
011300*                                 EMBALLAGEARTIKELNR FÖR Q3               
011400        05 KVQPACK-3         PIC Z(4)9.                                   
011500*                                 ANTAL I Q3 FÖRPACKNING                  
011600        05 IDARTNR-EMBQ4     OCCURS 2 TIMES                               
011700                             PIC Z(8)9.                                   
011800*                                 EMBALLAGEARTIKELNR FÖR Q4               
011900        05 KVQPACK-4         PIC Z(4)9.                                   
012000*                                 ANTAL I Q4 FÖRPACKNING                  
012100        05 IDKAT             OCCURS 3 TIMES                               
012200                             PIC X(6).                                    
012300*                                 KATALOGTILLHÖRIGHET   IDKAT-002         
012400     03 FLPCOO               PIC X.                                       
012500*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
012600*** END OF VILMAII-COPY LENGTH= 467 BYTES                                 
