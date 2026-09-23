000100 01  MOD-W6O13901.                                                        
000200*                                 COPYTEXT FÖR MID   W6013900             
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
000800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000900*                                 (0VVDLLLLK)                             
001000     03 MOD-IDDC-IN          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
001300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001400*                                 (0VVDLLLLK)                             
001500     03 MOD-IDDC-UT          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDKR-ENTER       PIC 9(5).                                    
001800*                                 KONTROLLRAPPORT NUMMER                  
001900     03 MOD-IDKR-NEXT        PIC 9(5).                                    
002000*                                 KONTROLLRAPPORT NUMMER                  
002100     03 MOD-IDKVAINF-ENTER   PIC 9(2).                                    
002200*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
002300     03 MOD-IDKVAINF-NEXT    PIC 9(2).                                    
002400*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
002500     03 MOD-IDARTNR          PIC Z(8)9.                                   
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-BEART            PIC X(25).                                   
002800     03 MOD-BELEVART         PIC X(30).                                   
002900*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003000     03 MOD-IDLEVNR          PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200     03 MOD-KVAVIS           PIC Z(6)9.                                   
003300*                                 AVISERAT ANTAL                          
003400     03 MOD-FLAGGA-KR-HOPP-ATTR                                           
003500                             PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-FLAGGA-KR-HOPP   PIC X.                                       
003800*                                 GOKDKÄND                                
003900     03 MOD-KVKVAPRIM        PIC Z(5)9.                                   
004000*                                 ANTAL TILL PRIMÄRKONTROLL               
004100     03 MOD-IDUSER-PRI-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-IDUSER-PRI       PIC X(8).                                    
004400*                                 ANVÄNDARENS SÄKERHETS ID                
004500     03 MOD-FLAGGA-PRI-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FLAGGA-PRI       PIC X.                                       
004800*                                 GOKDKÄND                                
004900     03 MOD-BEANST-ATTR      PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-BEANST           PIC X(25).                                   
005200*                                 ANSTÄLLDS NAMN                          
005300     03 MOD-KVKVASEK         PIC Z(5)9.                                   
005400*                                  ANTAL TILL SEKUNDÄRKONTROLL            
005500     03 MOD-IDUSER-SEK-ATTR  PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-IDUSER-SEK       PIC X(8).                                    
005800*                                 ANVÄNDARENS SÄKERHETS ID                
005900     03 MOD-FLAGGA-SEK-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-FLAGGA-SEK       PIC X.                                       
006200*                                 GOKDKÄND                                
006300     03 MOD-IDUSER-ADM-ATTR  PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-IDUSER-ADM       PIC X(8).                                    
006600*                                 ANVÄNDARENS SÄKERHETS ID                
006700     03 MOD-FLAGGA-ADM-ATTR  PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-FLAGGA-ADM       PIC X.                                       
007000*                                 GOKDKÄND                                
007100     03 MOD-KDKVATYP         PIC X(15).                                   
007200     03 MOD-ADKVAULG         PIC X(2).                                    
007300*                                 PLATS UNDERLAG KVAL.KONTROLL            
007400     03 MOD-UNDERLAG         PIC X(15).                                   
007500     03 MOD-SPEC-BEANST      PIC X(25).                                   
007600*                                 ANSTÄLLDS NAMN                          
007700     03 MOD-KONTROLL         PIC X(17).                                   
007800     03 MOD-IDKR             PIC 9(5).                                    
007900*                                 KONTROLLRAPPORT NUMMER                  
008000     03 MOD-FLAGGA-TEXT      PIC X(9).                                    
008100     03 MOD-FLAGGA-GODK-ATTR PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-FLAGGA-GODK      PIC X.                                       
008400*                                 GOKDKÄND                                
008500     03 MOD-EMPLID-TEXT      PIC X(8).                                    
008600     03 MOD-IDUSER-APR-ATTR  PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-IDUSER-APR       PIC X(8).                                    
008900*                                 ANVÄNDARENS SÄKERHETS ID                
009000     03 MOD-IDTFN            PIC X(20).                                   
009100*                                 TELEFONNUMMER EXTERNT                   
009200     03 MOD-TEKRFEL          OCCURS 4 TIMES                               
009300                             PIC X(70).                                   
009400*                                 FELBESKRIVNING I FRI TEXT               
009500     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
009600*                                 PRINTERPLACERING                        
009700     03 MOD-IDINLVGN-IN      PIC X(3).                                    
009800*                                 VAGNSIDENTITET                          
009900     03 MOD-IDINLVGN-UT      PIC X(3).                                    
010000*                                 VAGNSIDENTITET                          
010100     03 MOD-ADINLOMR-IN      PIC X(4).                                    
010200*                                 INLEVERANSOMRÅDE                        
010300     03 MOD-ADINLOMR-UT      PIC X(4).                                    
010400*                                 INLEVERANSOMRÅDE                        
010500     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
010600*                                 INLEVERANSOMRÅDE NÄSTA                  
010700     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
010800*                                 INLEVERANSOMRÅDE NÄSTA                  
010900     03 MOD-KDINLQ-IN        PIC X.                                       
011000*                                 INLEVERANSKÖTYP KOLLI/PARTI             
011100     03 MOD-KDINLQ-UT        PIC X.                                       
011200*                                 INLEVERANSKÖTYP KOLLI/PARTI             
011300     03 MOD-BEFT-FOM-IN      PIC X(2).                                    
011400*                                 FÖRPACKNINGSTYP                         
011500     03 MOD-BEFT-FOM-UT      PIC X(2).                                    
011600*                                 FÖRPACKNINGSTYP                         
011700     03 MOD-BEFT-TOM-IN      PIC X(2).                                    
011800*                                 FÖRPACKNINGSTYP                         
011900     03 MOD-BEFT-TOM-UT      PIC X(2).                                    
012000*                                 FÖRPACKNINGSTYP                         
012100     03 MOD-FLINLFB-IN       PIC X.                                       
012200*                                 VALD TILL FÖRBEHANDLING                 
012300     03 MOD-FLINLFB-UT       PIC X.                                       
012400*                                 VALD TILL FÖRBEHANDLING                 
012500     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
012600*                                 LEVERANTÖRNUMMER KOLLI                  
012700     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
012800*                                 LEVERANTÖRNUMMER KOLLI                  
012900     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
013000*                                 ODETTE KOLLINUMMER                      
013100     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
013200*                                 ODETTE KOLLINUMMER                      
013300     03 MOD-TEMFSINF         PIC X(55).                                   
013400*                                 INFORMATIONSMEDDELANDE                  
013500*** END OF VILMAII-COPY LENGTH= 767 BYTES                                 
