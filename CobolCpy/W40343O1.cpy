000100 01  RESP-W40343O1.                                                       
000200*                                 RESP-COPYTEXT FÖR W40343                
000300*                                                                         
000400     03 RESP-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RESP-ARB-ADDRESS     PIC X(11).                                   
000700     03 RESP-IDRADNR-FOM     PIC 9(5).                                    
000800*                                 RADNUMMER                               
000900     03 RESP-IDRADNR-TOM     PIC 9(5).                                    
001000*                                 RADNUMMER                               
001100     03 RESP-ORDER-INFO.                                                  
001200*                                                                         
001300        05 RESP-IDDISTR      PIC Z(5).                                    
001400*                                 DISTRIKTNUMMER                          
001500        05 RESP-IDKUNDNR     PIC Z(7).                                    
001600*                                 KUNDNUMMER                              
001700        05 RESP-IDDC         PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900        05 RESP-KDORDKL      PIC 9.                                       
002000*                                 ORDERKLASS                              
002100     03 RESP-KVORDRAD        PIC Z(5).                                    
002200*                                 ANTAL ORDERRADER                        
002300     03 RESP-TIPACKN         PIC 9(6).                                    
002400*                                 PACKNINGSDATUM         (ÅÅMMDD)         
002500     03 RESP-FLJANEJ-ALLA-ATTR                                            
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 RESP-FLJANEJ-ALLA    PIC X(3).                                    
002900*                                                     FLJANEJ-004         
003000     03 RESP-KDSVAR-ALLA     PIC X.                                       
003100*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003200     03 RESP-IDKOLLI-ALLA-ATTR                                            
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 RESP-IDKOLLI-ALLA    PIC Z(5).                                    
003600*                                 KOLLINUMMER                             
003700     03 RESP-IDKOLLI-NY-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 RESP-IDKOLLI-NY      PIC Z(5).                                    
004000*                                 KOLLINUMMER                             
004100     03 RESP-KDKOLLI-ATTR    PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 RESP-KDKOLLI         PIC X(8).                                    
004400*                                 KOLLIKOD                                
004500     03 RESP-KDEMBTYP-ATTR   PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 RESP-KDEMBTYP        PIC Z(3).                                    
004800*                                 EMBALLAGETYP       KDEMBTYP-002         
004900     03 RESP-VKORDBTO-ATTR   PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 RESP-VKORDBTO        PIC Z(6).Z.                                  
005200*                                 ORDERVIKT BRUTTO (KG)                   
005300     03 RESP-DIKOLLIL-ATTR   PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 RESP-DIKOLLIL        PIC Z(5).                                    
005600*                                 KOLLI-LÄNGD                             
005700     03 RESP-DIKOLLIB-ATTR   PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 RESP-DIKOLLIB        PIC Z(3).                                    
006000*                                 KOLLI-BREDD                             
006100     03 RESP-DIKOLLIH-ATTR   PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 RESP-DIKOLLIH        PIC Z(3).                                    
006400*                                 KOLLI-HÖJD                              
006500     03 RESP-RAD-GRUPP       OCCURS 500 TIMES.                            
006600*                                                                         
006700        05 RESP-FLNOLLAD-LINE                                             
006800                             PIC X.                                       
006900*                                 KOPPLING BORTTAGEN PÅ REG               
007000        05 RESP-IDPURAD-LINE PIC Z(3)9.                                   
007100*                                 RADNUMMER PÅ PACKUNDERLAG               
007200        05 RESP-IDARTNR-LINE PIC Z(9).                                    
007300*                                 ARTIKELNUMMER                           
007400        05 RESP-KVLEVART-LINE                                             
007500                             PIC Z(6)9.                                   
007600*                                 LEVERERAT ANTAL STYCK                   
007700        05 RESP-KDSVAR-LINE  PIC X.                                       
007800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
007900        05 RESP-IDKOLLI-LINE-ATTR                                         
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 RESP-IDKOLLI-LINE PIC Z(5).                                    
008300*                                 KOLLINUMMER                             
008400        05 RESP-KVLEVART-LINE-IN-ATTR                                     
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 RESP-KVLEVART-LINE-IN                                          
008800                             PIC Z(7).                                    
008900*                                 LEVERERAT ANTAL STYCK                   
009000*** END OF VILMAII-COPY LENGTH= 19114 BYTES                               
