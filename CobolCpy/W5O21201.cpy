000100 01  MOD-W5O21201.                                                        
000200*                                 MOD-COPYTEXT FÖR W5021200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FROM-DAREGDAT    PIC 9(8).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
000900     03 MOD-TOM-DAREGDAT     PIC 9(8).                                    
001000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001100     03 MOD-IDPGM            PIC X(8).                                    
001200*                                 PROGRAM IDENTITET                       
001300     03 MOD-KDEKHHT          PIC X(3).                                    
001400*                                 EKONOMISK HUVUDHÄNDELSE                 
001500     03 MOD-KDEKSHT          PIC X(3).                                    
001600*                                 EKONOMISK SUBHÄNDELSE                   
001700     03 MOD-KDEKNIVA         PIC X(5).                                    
001800*                                 EKONOMISK HÄNDELSENIVÅ                  
001900     03 MOD-BEEKHHT          PIC X(25).                                   
002000*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
002100     03 MOD-IDDISTR          PIC Z(5).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 MOD-IDKUNDNR         PIC Z(7).                                    
002400*                                 KUNDNUMMER                              
002500     03 MOD-IDVERGL          PIC X(10).                                   
002600*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
002700     03 MOD-DAVERDAT         PIC 9(8).                                    
002800*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
002900     03 MOD-IDARTNR          PIC Z(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-BEART            PIC X(25).                                   
003200*                                 ARTIKELBENÄMNING                        
003300     03 MOD-IDDC-SEND        PIC X(2).                                    
003400*                                 SÄNDANDE LAGER                          
003500     03 MOD-IDDC-REC         PIC X(2).                                    
003600*                                 MOTTAGANDE LAGER                        
003700     03 MOD-KDVALISO         PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900     03 MOD-PRKURS           PIC Z(5)9.9(5).                              
004000*                                 VALUTAKURS                              
004100     03 MOD-FLLSBOK          PIC X.                                       
004200*                                 LAGERAVBOKNING                          
004300     03 MOD-PRARTNTO         PIC -Z(6)9.9(2).                             
004400*                                 ARTIKELPRIS NETTO                       
004500     03 MOD-PRARTSTD         PIC -(7)9.9(2).                              
004600*                                 ARTIKELSTANDARDPRIS                     
004700     03 MOD-PRARTSJK         PIC -(7)9.9(2).                              
004800*                                 ARTIKELNS SJÄLVKOSTNAD                  
004900     03 MOD-PRINK            PIC Z(6)9.9(2).                              
005000*                                 INKÖPSPRIS                              
005100     03 MOD-DAREGDAT         PIC 9(8).                                    
005200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
005300     03 MOD-TIKLOCK          PIC 9(9).                                    
005400*                                 KLOCKSLAG (TTMMSSTH)                    
005500     03 MOD-PRDIRLON         PIC Z(3)9.9(3).                              
005600*                                 DIREKT LÖN                              
005700     03 MOD-IDPGM-NY         PIC X(8).                                    
005800*                                 PROGRAM IDENTITET                       
005900     03 MOD-PRDMTRL          PIC Z(5)9.9(3).                              
006000*                                 DIREKT MATERIAL                         
006100     03 MOD-IDTRANS-NY       PIC X(4).                                    
006200*                                 BILDNUMMER                              
006300     03 MOD-PROVRPAL         PIC Z(3)9.9(3).                              
006400*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
006500     03 MOD-IDUSER           PIC X(8).                                    
006600*                                 ANVÄNDARENS SÄKERHETS ID                
006700     03 MOD-PRHEMTAG         PIC Z(6)9.9(2).                              
006800*                                 HEMTAGNINGSKOSTNAD                      
006900     03 MOD-SUBEL            PIC -Z(8)9.9(2).                             
007000*                                 SUMMABELOPP                             
007100     03 MOD-DAREGDAT-FEL     PIC 9(8).                                    
007200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
007300     03 MOD-KVANTAL          PIC -(7)9.                                   
007400*                                 ANTAL                                   
007500     03 MOD-BEFEL            PIC X(50).                                   
007600*                                 FELTEXT                                 
007700     03 MOD-TEMFSINF         PIC X(55).                                   
007800*                                 INFORMATIONSMEDDELANDE                  
007900*** END OF VILMAII-COPY LENGTH= 438 BYTES                                 
