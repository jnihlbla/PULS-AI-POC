000100 01  W4O34301.                                                            
000200*                                 COPYTEXT FÖR MOD W4O34301               
000300*                                                                         
000400     03 TRANS-NUMMER         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MESSAGE-RAD1         PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 IDPRODNR-IN-ATTR     PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 IDPRODNR-IN          PIC X(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200     03 IDKOLLI-IN-ATTR      PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 IDKOLLI-IN           PIC X(5).                                    
001500*                                 KOLLINUMMER                             
001600     03 IDDC-IN-ATTR         PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 IDDC-IN              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 IDPRODNR-UT          PIC X(7).                                    
002100*                                 PRODUKTIONSNUMMER                       
002200     03 IDKOLLI-UT           PIC X(5).                                    
002300*                                 KOLLINUMMER                             
002400     03 IDDC-UT              PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600     03 IDRADNR-FOM          PIC 9(5).                                    
002700*                                 RADNUMMER                               
002800     03 IDRADNR-TOM          PIC 9(5).                                    
002900*                                 RADNUMMER                               
003000     03 FLNOLLAD-TAB.                                                     
003100*                                                                         
003200        05 FLNOLLAD          OCCURS 16 TIMES                              
003300                             PIC X.                                       
003400*                                 KOPPLING BORTTAGEN PÅ REG               
003500     03 ORDER-INFO.                                                       
003600*                                                                         
003700        05 IDDISTR           PIC Z(5).                                    
003800*                                 DISTRIKTNUMMER                          
003900        05 IDKUNDNR          PIC Z(7).                                    
004000*                                 KUNDNUMMER                              
004100        05 FILLERINF1        PIC X.                                       
004200        05 IDDC              PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400        05 FILLERINF2        PIC X(2).                                    
004500        05 KDORDKL           PIC 9.                                       
004600*                                 ORDERKLASS                              
004700     03 KVORDRAD             PIC Z(5).                                    
004800*                                 ANTAL ORDERRADER                        
004900     03 TIPACKN              PIC 9(6).                                    
005000*                                 PACKNINGSDATUM         (ÅÅMMDD)         
005100     03 FLJANEJ-ALLA-ATTR    PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 FLJANEJ-ALLA         PIC X(3).                                    
005400*                                                     FLJANEJ-004         
005500     03 KDSVAR-ALLA          PIC X.                                       
005600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
005700     03 IDKOLLI-ALLA-ATTR    PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 IDKOLLI-ALLA         PIC Z(5).                                    
006000*                                 KOLLINUMMER                             
006100     03 RAD-GRUPP            OCCURS 16 TIMES.                             
006200*                                                                         
006300        05 IDPURAD           PIC Z(3)9.                                   
006400*                                 RADNUMMER PÅ PACKUNDERLAG               
006500        05 IDARTNR           PIC Z(9).                                    
006600*                                 ARTIKELNUMMER                           
006700        05 KVLEVART-UT       PIC Z(6)9.                                   
006800*                                 LEVERERAT ANTAL STYCK                   
006900        05 KDSVAR-RAD        PIC X.                                       
007000*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
007100        05 IDKOLLI-RAD-ATTR  PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 IDKOLLI-RAD       PIC Z(5).                                    
007400*                                 KOLLINUMMER                             
007500        05 KVLEVART-IN-ATTR  PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 KVLEVART-IN       PIC Z(7).                                    
007800*                                 LEVERERAT ANTAL STYCK                   
007900     03 IDKOLLI-NY-ATTR      PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 IDKOLLI-NY           PIC Z(5).                                    
008200*                                 KOLLINUMMER                             
008300     03 KDKOLLI-ATTR         PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 KDKOLLI              PIC X(8).                                    
008600*                                 KOLLIKOD                                
008700     03 KDEMBTYP-ATTR        PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 KDEMBTYP             PIC Z(3).                                    
009000*                                 EMBALLAGETYP       KDEMBTYP-002         
009100     03 VKORDBTO-ATTR        PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 VKORDBTO             PIC Z(6).Z.                                  
009400*                                 ORDERVIKT BRUTTO (KG)                   
009500     03 DIKOLLIL-ATTR        PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 DIKOLLIL             PIC Z(5).                                    
009800*                                 KOLLI-LÄNGD                             
009900     03 DIKOLLIB-ATTR        PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 DIKOLLIB             PIC Z(3).                                    
010200*                                 KOLLI-BREDD                             
010300     03 DIKOLLIH-ATTR        PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 DIKOLLIH             PIC Z(3).                                    
010600*                                 KOLLI-HÖJD                              
010700     03 MESSAGE-RAD23        PIC X(79).                                   
010800*                                 MEDDELANDEFÄLT PÅ RAD 23                
010900*** END OF VILMAII-COPY LENGTH= 866 BYTES                                 
