000100 01  W4O66401.                                                            
000200*                                 MODCOPYTEXT TILL W40664.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDTRPTNR-IN          PIC X(3).                                    
000800*                                 TRANSPORTIDENTITET                      
000900     03 IDTRPTNR-UT          PIC X(3).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 IDLBBET-IN           PIC X(12).                                   
001200*                                 LASTBÄRARBETECKNING                     
001300     03 IDLBBET-UT           PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500     03 FLFARLIG-IN          PIC X.                                       
001600*                                 FARLIGT GODS-FLAGGA                     
001700     03 FLFARLIG-UT          PIC X.                                       
001800*                                 FARLIGT GODS-FLAGGA                     
001900     03 IDDC-IN              PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 IDDC-UT              PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 IDDISTR-ENTER        PIC X(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 IDKUNDNR-ENTER       PIC X(6).                                    
002600*                                 KUNDNUMMER                              
002700     03 KDFAKTYP-ENTER       PIC X.                                       
002800*                                 FAKTURATYP                              
002900     03 IDORDNR7-ENTER       PIC X(7).                                    
003000*                                 ORDERNUMMER                             
003100     03 IDPRODNR-ENTER       PIC X(7).                                    
003200*                                 PRODUKTIONSNUMMER                       
003300     03 IDKOLLI-ENTER        PIC X(5).                                    
003400*                                 KOLLINUMMER                             
003500     03 IDDISTR-NEXT         PIC X(4).                                    
003600*                                 DISTRIKTNUMMER                          
003700     03 IDKUNDNR-NEXT        PIC X(6).                                    
003800*                                 KUNDNUMMER                              
003900     03 KDFAKTYP-NEXT        PIC X.                                       
004000*                                 FAKTURATYP                              
004100     03 IDORDNR7-NEXT        PIC X(7).                                    
004200*                                 ORDERNUMMER                             
004300     03 IDPRODNR-NEXT        PIC X(7).                                    
004400*                                 PRODUKTIONSNUMMER                       
004500     03 IDKOLLI-NEXT         PIC X(5).                                    
004600*                                 KOLLINUMMER                             
004700     03 SUMMOR.                                                           
004800*                                                                         
004900        05 KVKOLLI-TOT       PIC Z(3)9.                                   
005000*                                 ANTAL KOLLI                             
005100        05 VKORDBTO-TOT      PIC Z(5)9.9.                                 
005200*                                 ORDERVIKT BRUTTO (KG)                   
005300        05 VLORDBTO-TOT      PIC Z(3)9.9(3).                              
005400*                                 ORDERVOLYM BRUTTO (M3)                  
005500        05 SUORDV-TOT        PIC Z(8)9.9(2).                              
005600*                                 SUMMA ORDERVÄRDE                        
005700     03 FLAVSLUTA-ATTR       PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 FLAVSLUTA            PIC X.                                       
006000*                                 ALLMÄN FLAGGA                           
006100     03 IDDISTR-DOLD         PIC 9(4).                                    
006200*                                 DISTRIKTNUMMER                          
006300     03 TELSTDOK-ATTR        PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 TELSTDOK             PIC X(9).                                    
006600     03 FLLSTDOK-ATTR        PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 FLLSTDOK             PIC X.                                       
006900*                                 ALLMÄN FLAGGA                           
007000     03 TETRPDOK-ATTR        PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 TETRPDOK             PIC X(22).                                   
007300     03 FLTRPDOK-ATTR        PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 FLTRPDOK             PIC X.                                       
007600*                                 ALLMÄN FLAGGA                           
007700     03 TEPROFORMA-ATTR      PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 TEPROFORMA           PIC X(8).                                    
008000     03 FLPROFORMA-ATTR      PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 FLPROFORMA           PIC X.                                       
008300*                                 ALLMÄN FLAGGA                           
008400     03 RADER                OCCURS 10 TIMES.                             
008500*                                                                         
008600        05 FLBACKA-RAD-ATTR  PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 FLBACKA-RAD       PIC X.                                       
008900*                                 ALLMÄN FLAGGA                           
009000        05 IDDISTR           PIC 9(4).                                    
009100*                                 DISTRIKTNUMMER                          
009200        05 IDKUNDNR          PIC Z(5)9.                                   
009300*                                 KUNDNUMMER                              
009400        05 KDFAKTYP          PIC X.                                       
009500*                                 FAKTURATYP                              
009600        05 IDORDNR7          PIC Z(6)9.                                   
009700*                                 ORDERNUMMER                             
009800        05 IDKOLLI           PIC 9(5).                                    
009900*                                 KOLLINUMMER                             
010000        05 TIRFS             PIC 9(10).                                   
010100*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
010200        05 KDKOLLI           PIC X(8).                                    
010300*                                 KOLLIKOD                                
010400        05 VKORDBTO          PIC Z(5)9.9.                                 
010500*                                 ORDERVIKT BRUTTO (KG)                   
010600        05 VLORDBTO          PIC Z(3)9.9(3).                              
010700*                                 ORDERVOLYM BRUTTO (M3)                  
010800        05 IDPSN             PIC X(4).                                    
010900*                                 PROPER SHIPPING NAME                    
011000        05 IDPRODNR          PIC 9(7).                                    
011100*                                 PRODUKTIONSNUMMER                       
011200     03 IDKOLLI-SAMP-TAB.                                                 
011300*                                                                         
011400        05 IDKOLLI-SAMP      OCCURS 10 TIMES                              
011500                             PIC 9(5).                                    
011600*                                 SAMPACKNINGSKOLLINUMMER                 
011700     03 TEMFSINF             PIC X(55).                                   
011800*                                 INFORMATIONSMEDDELANDE                  
011900*** END OF VILMAII-COPY LENGTH= 1048 BYTES                                
