000100 01  MOD-W6O10601.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O10601                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-ADINLOMR-IN      PIC X(4).                                    
000900*                                 INLEVERANSOMRÅDE                        
001000     03 MOD-ADINLOMR-UT      PIC X(4).                                    
001100*                                 INLEVERANSOMRÅDE                        
001200     03 MOD-KDINLOMR-IN      PIC X(3).                                    
001300*                                 TYP AV INLEVERANSOMRÅDE                 
001400     03 MOD-KDINLOMR-UT      PIC X(3).                                    
001500*                                 TYP AV INLEVERANSOMRÅDE                 
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-INPUT.                                                        
002100*                                 INDATA FÖR UPPDATERING                  
002200        05 MOD-ADINLOMR-PAR-IN-ATTR                                       
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-ADINLOMR-PAR-IN                                            
002600                             PIC X(4).                                    
002700*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
002800        05 MOD-ADINLOMR-PAR-UT-ATTR                                       
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-ADINLOMR-PAR-UT                                            
003200                             PIC X(4).                                    
003300*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
003400        05 MOD-KDINLUPF-IN-ATTR                                           
003500                             PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-KDINLUPF-IN   PIC X(4).                                    
003800*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
003900        05 MOD-KDINLUPF-UT-ATTR                                           
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-KDINLUPF-UT   PIC X(4).                                    
004300*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
004400        05 MOD-ADINLOMR-BO-IN-ATTR                                        
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-ADINLOMR-BO-IN                                             
004800                             PIC X(4).                                    
004900*                                 BUFFERTOMRÅDE                           
005000        05 MOD-ADINLOMR-BO-UT-ATTR                                        
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-ADINLOMR-BO-UT                                             
005400                             PIC X(4).                                    
005500*                                 BUFFERTOMRÅDE                           
005600        05 MOD-ADINLOMR-LPL-IN-ATTR                                       
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-ADINLOMR-LPL-IN                                            
006000                             PIC X(4).                                    
006100*                                 LOSSNINGSPLATS                          
006200        05 MOD-ADINLOMR-LPL-UT-ATTR                                       
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-ADINLOMR-LPL-UT                                            
006600                             PIC X(4).                                    
006700*                                 LOSSNINGSPLATS                          
006800        05 MOD-KDLORAPP-IN-ATTR                                           
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100        05 MOD-KDLORAPP-IN   PIC X.                                       
007200*                                 KOD FÖR R32-RAPPORTERING                
007300        05 MOD-TEXT1-IN-ATTR PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-TEXT1-IN      PIC X(17).                                   
007600        05 MOD-KDLORAPP-UT-ATTR                                           
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-KDLORAPP-UT   PIC X.                                       
008000*                                 KOD FÖR R32-RAPPORTERING                
008100        05 MOD-TEXT1-UT-ATTR PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-TEXT1-UT      PIC X(17).                                   
008400        05 MOD-ADPLATS-FOM-IN-ATTR                                        
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 MOD-ADPLATS-FOM-IN                                             
008800                             PIC X(5).                                    
008900*                                 LAGERPLATSNUMMER                        
009000        05 MOD-ADPLATS-TOM-IN-ATTR                                        
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-ADPLATS-TOM-IN                                             
009400                             PIC X(5).                                    
009500*                                 LAGERPLATSNUMMER                        
009600        05 MOD-ADPLATS-FOM-UT-ATTR                                        
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900        05 MOD-ADPLATS-FOM-UT                                             
010000                             PIC X(5).                                    
010100*                                 LAGERPLATSNUMMER                        
010200        05 MOD-ADPLATS-TOM-UT-ATTR                                        
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500        05 MOD-ADPLATS-TOM-UT                                             
010600                             PIC X(5).                                    
010700*                                 LAGERPLATSNUMMER                        
010800        05 MOD-ADGANG-FOM-IN-ATTR                                         
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-ADGANG-FOM-IN PIC X(2).                                    
011200*                                 GÅNG                                    
011300        05 MOD-ADGANG-TOM-IN-ATTR                                         
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 MOD-ADGANG-TOM-IN PIC X(2).                                    
011700*                                 GÅNG                                    
011800        05 MOD-ADGANG-FOM-UT-ATTR                                         
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100        05 MOD-ADGANG-FOM-UT PIC 9(2).                                    
012200*                                 GÅNG                                    
012300        05 MOD-ADGANG-TOM-UT-ATTR                                         
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-ADGANG-TOM-UT PIC 9(2).                                    
012700*                                 GÅNG                                    
012800        05 MOD-FLLOLL-IN-ATTR                                             
012900                             PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100        05 MOD-FLLOLL-IN     PIC X.                                       
013200*                                 SKAPA LOSSNINGSLISTEFLAGGA              
013300        05 MOD-FLLOLL-UT-ATTR                                             
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600        05 MOD-FLLOLL-UT     PIC X.                                       
013700*                                 SKAPA LOSSNINGSLISTEFLAGGA              
013800        05 MOD-FLCDOMR-IN-ATTR                                            
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100        05 MOD-FLCDOMR-IN    PIC X.                                       
014200*                                 SKAPA LOSSNINGSLISTEFLAGGA              
014300        05 MOD-FLCDOMR-UT-ATTR                                            
014400                             PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600        05 MOD-FLCDOMR-UT    PIC X.                                       
014700*                                 SKAPA LOSSNINGSLISTEFLAGGA              
014800        05 MOD-IDPERSON-ANSV-IN-ATTR                                      
014900                             PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100        05 MOD-IDPERSON-ANSV-IN                                           
015200                             PIC 9(3).                                    
015300*                                 PERSONKOD                               
015400        05 MOD-IDPERSON-ANSV-UT-ATTR                                      
015500                             PIC X(2).                                    
015600*                                 MFS ATTRIBUTFÄLT                        
015700        05 MOD-IDPERSON-ANSV-UT                                           
015800                             PIC 9(3).                                    
015900*                                 PERSONKOD                               
016000        05 MOD-BEINIT-ANSV   PIC X(5).                                    
016100*                                 INITIALER FÖR EN PERSON                 
016200        05 MOD-IDLEVNR-IN-ATTR                                            
016300                             PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500        05 MOD-IDLEVNR-IN    PIC X(5).                                    
016600*                                 LEVERANTÖRNUMMER                        
016700        05 MOD-IDLEVNR-UT-ATTR                                            
016800                             PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000        05 MOD-IDLEVNR-UT    PIC X(5).                                    
017100*                                 LEVERANTÖRNUMMER                        
017200        05 MOD-IDPERSON-FORP-IN-ATTR                                      
017300                             PIC X(2).                                    
017400*                                 MFS ATTRIBUTFÄLT                        
017500        05 MOD-IDPERSON-FORP-IN                                           
017600                             PIC 9(3).                                    
017700*                                 PERSONKOD                               
017800        05 MOD-IDPERSON-FORP-UT-ATTR                                      
017900                             PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100        05 MOD-IDPERSON-FORP-UT                                           
018200                             PIC 9(3).                                    
018300*                                 PERSONKOD                               
018400        05 MOD-BEINIT-FORP   PIC X(5).                                    
018500*                                 INITIALER FÖR EN PERSON                 
018600        05 MOD-ADINLOMR-PRT-IN-ATTR                                       
018700                             PIC X(2).                                    
018800*                                 MFS ATTRIBUTFÄLT                        
018900        05 MOD-ADINLOMR-PRT-IN                                            
019000                             PIC X(4).                                    
019100*                                 PRINTERPLACERING                        
019200        05 MOD-ADINLOMR-PRT-UT-ATTR                                       
019300                             PIC X(2).                                    
019400*                                 MFS ATTRIBUTFÄLT                        
019500        05 MOD-ADINLOMR-PRT-UT                                            
019600                             PIC X(4).                                    
019700*                                 PRINTERPLACERING                        
019800        05 MOD-IDPERSON-KVAL-IN-ATTR                                      
019900                             PIC X(2).                                    
020000*                                 MFS ATTRIBUTFÄLT                        
020100        05 MOD-IDPERSON-KVAL-IN                                           
020200                             PIC 9(3).                                    
020300*                                 PERSONKOD                               
020400        05 MOD-IDPERSON-KVAL-UT-ATTR                                      
020500                             PIC X(2).                                    
020600*                                 MFS ATTRIBUTFÄLT                        
020700        05 MOD-IDPERSON-KVAL-UT                                           
020800                             PIC 9(3).                                    
020900*                                 PERSONKOD                               
021000        05 MOD-BEINIT-KVAL   PIC X(5).                                    
021100*                                 INITIALER FÖR EN PERSON                 
021200        05 MOD-IDAVD-DAG-IN-ATTR                                          
021300                             PIC X(2).                                    
021400*                                 MFS ATTRIBUTFÄLT                        
021500        05 MOD-IDAVD-DAG-IN  PIC X(5).                                    
021600*                                 DEN ANSTÄLLDES AVDELNING/DAG            
021700        05 MOD-IDGRUPP-DAG-IN-ATTR                                        
021800                             PIC X(2).                                    
021900*                                 MFS ATTRIBUTFÄLT                        
022000        05 MOD-IDGRUPP-DAG-IN                                             
022100                             PIC X(2).                                    
022200*                                 DEN ANSTÄLLDES GRUPPID/DAG              
022300        05 MOD-IDAVD-DAG-UT  PIC X(5).                                    
022400*                                 DEN ANSTÄLLDES AVDELNING/DAG            
022500        05 MOD-IDGRUPP-DAG-UT                                             
022600                             PIC X(2).                                    
022700*                                 DEN ANSTÄLLDES GRUPPID/DAG              
022800        05 MOD-IDAVD-NATT-IN-ATTR                                         
022900                             PIC X(2).                                    
023000*                                 MFS ATTRIBUTFÄLT                        
023100        05 MOD-IDAVD-NATT-IN PIC X(5).                                    
023200*                                 DEN ANSTÄLLDES AVDELNING/NATT           
023300        05 MOD-IDGRUPP-NATT-IN-ATTR                                       
023400                             PIC X(2).                                    
023500*                                 MFS ATTRIBUTFÄLT                        
023600        05 MOD-IDGRUPP-NATT-IN                                            
023700                             PIC X(2).                                    
023800*                                 DEN ANSTÄLLDES GRUPPID/NATT             
023900        05 MOD-IDAVD-NATT-UT PIC X(5).                                    
024000*                                 DEN ANSTÄLLDES AVDELNING/NATT           
024100        05 MOD-IDGRUPP-NATT-UT                                            
024200                             PIC X(2).                                    
024300*                                 DEN ANSTÄLLDES GRUPPID/NATT             
024400        05 MOD-FLKVARED-IN-ATTR                                           
024500                             PIC X(2).                                    
024600*                                 MFS ATTRIBUTFÄLT                        
024700        05 MOD-FLKVARED-IN   PIC X.                                       
024800*                                 REDUCERAD KONTROLL FLAGGA               
024900        05 MOD-FLKVARED-UT-ATTR                                           
025000                             PIC X(2).                                    
025100*                                 MFS ATTRIBUTFÄLT                        
025200        05 MOD-FLKVARED-UT   PIC X.                                       
025300*                                 REDUCERAD KONTROLL FLAGGA               
025400        05 MOD-FLKNTRGK-IN-ATTR                                           
025500                             PIC X(2).                                    
025600*                                 MFS ATTRIBUTFÄLT                        
025700        05 MOD-FLKNTRGK-IN   PIC X.                                       
025800*                                 OMPLACERING GODK. KONTROLL J/N          
025900        05 MOD-FLKNTRGK-UT-ATTR                                           
026000                             PIC X(2).                                    
026100*                                 MFS ATTRIBUTFÄLT                        
026200        05 MOD-FLKNTRGK-UT   PIC X.                                       
026300*                                 OMPLACERING GODK. KONTROLL J/N          
026400        05 MOD-FLSVAR-IN-ATTR                                             
026500                             PIC X(2).                                    
026600*                                 MFS ATTRIBUTFÄLT                        
026700        05 MOD-FLSVAR-IN     PIC X.                                       
026800*                                 ALLMÄN SVARSFLAGGA                      
026900        05 MOD-FLSVAR-UT-ATTR                                             
027000                             PIC X(2).                                    
027100*                                 MFS ATTRIBUTFÄLT                        
027200        05 MOD-FLSVAR-UT     PIC X.                                       
027300*                                 ALLMÄN SVARSFLAGGA                      
027400        05 MOD-FLEXCP-IN-ATTR                                             
027500                             PIC X(2).                                    
027600*                                 MFS ATTRIBUTFÄLT                        
027700        05 MOD-FLEXCP-IN     PIC X.                                       
027800*                                 ALLMÄN FLAGGA FÖR UNDANTAG              
027900        05 MOD-FLEXCP-UT-ATTR                                             
028000                             PIC X(2).                                    
028100*                                 MFS ATTRIBUTFÄLT                        
028200        05 MOD-FLEXCP-UT     PIC X.                                       
028300*                                 ALLMÄN FLAGGA FÖR UNDANTAG              
028400        05 MOD-KVTID-NORM-IN-ATTR                                         
028500                             PIC X(2).                                    
028600*                                 MFS ATTRIBUTFÄLT                        
028700        05 MOD-KVTID-NORM-IN PIC 9(4).                                    
028800*                                 NORMAL MÅLTID FÖR GODSPLACERING         
028900        05 MOD-KVTID-NORM-UT-ATTR                                         
029000                             PIC X(2).                                    
029100*                                 MFS ATTRIBUTFÄLT                        
029200        05 MOD-KVTID-NORM-UT PIC 9(4).                                    
029300*                                 NORMAL MÅLTID FÖR GODSPLACERING         
029400        05 MOD-KVTID-NORMTOT-IN-ATTR                                      
029500                             PIC X(2).                                    
029600*                                 MFS ATTRIBUTFÄLT                        
029700        05 MOD-KVTID-NORMTOT-IN                                           
029800                             PIC 9(4).                                    
029900*                                 NORMTOTAL MÅLTID GODSPLACERING          
030000        05 MOD-KVTID-NORMTOT-UT-ATTR                                      
030100                             PIC X(2).                                    
030200*                                 MFS ATTRIBUTFÄLT                        
030300        05 MOD-KVTID-NORMTOT-UT                                           
030400                             PIC 9(4).                                    
030500*                                 NORMTOTAL MÅLTID GODSPLACERING          
030600        05 MOD-KVTID-PRIO-IN-ATTR                                         
030700                             PIC X(2).                                    
030800*                                 MFS ATTRIBUTFÄLT                        
030900        05 MOD-KVTID-PRIO-IN PIC 9(4).                                    
031000*                                 PRIO MÅLTID FÖR GODSPLACERING           
031100        05 MOD-KVTID-PRIO-UT-ATTR                                         
031200                             PIC X(2).                                    
031300*                                 MFS ATTRIBUTFÄLT                        
031400        05 MOD-KVTID-PRIO-UT PIC 9(4).                                    
031500*                                 PRIO MÅLTID FÖR GODSPLACERING           
031600        05 MOD-KVTID-PRIOTOT-IN-ATTR                                      
031700                             PIC X(2).                                    
031800*                                 MFS ATTRIBUTFÄLT                        
031900        05 MOD-KVTID-PRIOTOT-IN                                           
032000                             PIC 9(4).                                    
032100*                                 PRIOTOTAL MÅLTID GODSPLACERING          
032200        05 MOD-KVTID-PRIOTOT-UT-ATTR                                      
032300                             PIC X(2).                                    
032400*                                 MFS ATTRIBUTFÄLT                        
032500        05 MOD-KVTID-PRIOTOT-UT                                           
032600                             PIC 9(4).                                    
032700*                                 PRIOTOTAL MÅLTID GODSPLACERING          
032800        05 MOD-KVTID-NTCDC-IN-ATTR                                        
032900                             PIC X(2).                                    
033000*                                 MFS ATTRIBUTFÄLT                        
033100        05 MOD-KVTID-NTCDC-IN                                             
033200                             PIC 9(4).                                    
033300*                                 NORMTOTAL MÅLTID GODSPLAC.(CDC)         
033400        05 MOD-KVTID-NTCDC-UT-ATTR                                        
033500                             PIC X(2).                                    
033600*                                 MFS ATTRIBUTFÄLT                        
033700        05 MOD-KVTID-NTCDC-UT                                             
033800                             PIC 9(4).                                    
033900*                                 NORMTOTAL MÅLTID GODSPLAC.(CDC)         
034000        05 MOD-KVTID-PTCDC-IN-ATTR                                        
034100                             PIC X(2).                                    
034200*                                 MFS ATTRIBUTFÄLT                        
034300        05 MOD-KVTID-PTCDC-IN                                             
034400                             PIC 9(4).                                    
034500*                                 PRIOTOTAL MÅLTID GODSPLAC.(CDC)         
034600        05 MOD-KVTID-PTCDC-UT-ATTR                                        
034700                             PIC X(2).                                    
034800*                                 MFS ATTRIBUTFÄLT                        
034900        05 MOD-KVTID-PTCDC-UT                                             
035000                             PIC 9(4).                                    
035100*                                 PRIOTOTAL MÅLTID GODSPLAC.(CDC)         
035200        05 MOD-KVTID-NTSVS-IN-ATTR                                        
035300                             PIC X(2).                                    
035400*                                 MFS ATTRIBUTFÄLT                        
035500        05 MOD-KVTID-NTSVS-IN                                             
035600                             PIC 9(4).                                    
035700*                                 NORMTOTAL MÅLTID GODSPLAC.(SVS)         
035800        05 MOD-KVTID-NTSVS-UT-ATTR                                        
035900                             PIC X(2).                                    
036000*                                 MFS ATTRIBUTFÄLT                        
036100        05 MOD-KVTID-NTSVS-UT                                             
036200                             PIC 9(4).                                    
036300*                                 NORMTOTAL MÅLTID GODSPLAC.(SVS)         
036400        05 MOD-KVTID-PTSVS-IN-ATTR                                        
036500                             PIC X(2).                                    
036600*                                 MFS ATTRIBUTFÄLT                        
036700        05 MOD-KVTID-PTSVS-IN                                             
036800                             PIC 9(4).                                    
036900*                                 PRIOTOTAL MÅLTID GODSPLAC.(SVS)         
037000        05 MOD-KVTID-PTSVS-UT-ATTR                                        
037100                             PIC X(2).                                    
037200*                                 MFS ATTRIBUTFÄLT                        
037300        05 MOD-KVTID-PTSVS-UT                                             
037400                             PIC 9(4).                                    
037500*                                 PRIOTOTAL MÅLTID GODSPLAC.(SVS)         
037600     03 MOD-TEMFSINF         PIC X(55).                                   
037700*                                 INFORMATIONSMEDDELANDE                  
037800*** END OF VILMAII-COPY LENGTH= 492 BYTES                                 
