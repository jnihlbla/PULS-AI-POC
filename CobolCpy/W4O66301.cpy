000100 01  W4O66301.                                                            
000200*                                 MODCOPYTEXT TILL W40663.                
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
002300     03 SUMMOR.                                                           
002400*                                                                         
002500        05 KVKOLLI-VALB      PIC Z(3)9.                                   
002600*                                 ANTAL KOLLI                             
002700        05 VKORDBTO-VALB     PIC Z(5)9.9.                                 
002800*                                 ORDERVIKT BRUTTO (KG)                   
002900        05 VLORDBTO-VALB     PIC Z(3)9.9(3).                              
003000*                                 ORDERVOLYM BRUTTO (M3)                  
003100        05 KVKOLLI-TOT       PIC Z(3)9.                                   
003200*                                 ANTAL KOLLI                             
003300        05 VKORDBTO-TOT      PIC Z(6)9.9.                                 
003400*                                 ORDERVIKT BRUTTO (KG)                   
003500        05 VLORDBTO-TOT      PIC Z(4)9.9(3).                              
003600*                                 ORDERVOLYM BRUTTO (M3)                  
003700        05 KVKOLLI-VALD      PIC Z(3)9.                                   
003800*                                 ANTAL KOLLI                             
003900        05 VKORDBTO-VALD     PIC Z(5)9.9.                                 
004000*                                 ORDERVIKT BRUTTO (KG)                   
004100        05 VLORDBTO-VALD     PIC Z(3)9.9(3).                              
004200*                                 ORDERVOLYM BRUTTO (M3)                  
004300     03 FLSIDLAST-ATTR       PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 FLSIDLAST            PIC X.                                       
004600*                                 LASTA HEL SIDA?                         
004700     03 RADER                OCCURS 10 TIMES.                             
004800*                                                                         
004900        05 FLLASTA-RAD-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 FLLASTA-RAD       PIC X.                                       
005200*                                 SKA ORDERN LASTAS ?                     
005300        05 IDDISTR           PIC 9(4).                                    
005400*                                 DISTRIKTNUMMER                          
005500        05 IDKUNDNR          PIC Z(5)9.                                   
005600*                                 KUNDNUMMER                              
005700        05 IDORDNR7-ATTR     PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 IDORDNR7          PIC Z(6)9.                                   
006000*                                 ORDERNUMMER                             
006100        05 IDKOLLI           PIC 9(5).                                    
006200*                                 KOLLINUMMER                             
006300        05 TIRFS             PIC 9(10).                                   
006400*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
006500        05 KDKOLLI           PIC X(8).                                    
006600*                                 KOLLIKOD                                
006700        05 VKORDBTO          PIC Z(5)9.9.                                 
006800*                                 ORDERVIKT BRUTTO (KG)                   
006900        05 VLORDBTO          PIC Z(3)9.9(3).                              
007000*                                 ORDERVOLYM BRUTTO (M3)                  
007100        05 IDPSN             PIC X(4).                                    
007200*                                 PROPER SHIPPING NAME                    
007300        05 IDPRODNR          PIC 9(7).                                    
007400*                                 PRODUKTIONSNUMMER                       
007500     03 TEMFSINF             PIC X(55).                                   
007600*                                 INFORMATIONSMEDDELANDE                  
007700*** END OF VILMAII-COPY LENGTH= 920 BYTES                                 
