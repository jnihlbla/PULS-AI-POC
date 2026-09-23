000100 01  MOD-W4O11101.                                                        
000200*                                 MODCOPYTEXT TILL W40111.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 MOD-KDKOLLI-IN       PIC X(8).                                    
000800*                                 KOLLIKOD                                
000900     03 MOD-KDKOLLI-UT       PIC X(8).                                    
001000*                                 KOLLIKOD                                
001100     03 MOD-KDKOLLI-SPAR     PIC X(8).                                    
001200*                                 KOLLIKOD                                
001300     03 MOD-INDATA.                                                       
001400*                                 RAD INNEHÅLLANDE INDATAPOSTEN           
001500        05 MOD-KDKOLLI-INP-ATTR                                           
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 MOD-KDKOLLI-INP   PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000        05 MOD-KDEMBTYP-INP-ATTR                                          
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-KDEMBTYP-INP  PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500        05 MOD-DIKOLLIL-INP-ATTR                                          
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-DIKOLLIL-INP  PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000        05 MOD-DIKOLLIB-INP-ATTR                                          
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-DIKOLLIB-INP  PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500        05 MOD-DIKOLLIH-INP-ATTR                                          
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-DIKOLLIH-INP  PIC X(2).                                    
003900*                                 MFS BEHANDLING AV INPUTFÄLT             
004000        05 MOD-VKTARA-INP-ATTR                                            
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-VKTARA-INP    PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500        05 MOD-KDKOLLID-INP-ATTR                                          
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-KDKOLLID-INP  PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000        05 MOD-KVPALLAR-INP-ATTR                                          
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-KVPALLAR-INP  PIC X(2).                                    
005400*                                 MFS BEHANDLING AV INPUTFÄLT             
005500        05 MOD-KVRAM-INP-ATTR                                             
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-KVRAM-INP     PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 MOD-KVEMBSPA-INP-ATTR                                          
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-KVEMBSPA-INP  PIC X(2).                                    
006400*                                 MFS BEHANDLING AV INPUTFÄLT             
006500        05 MOD-KVLOCK-INP-ATTR                                            
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-KVLOCK-INP    PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000        05 MOD-KVINTPALL-INP-ATTR                                         
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-KVINTPALL-INP PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500        05 MOD-KDANDR-INP-ATTR                                            
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KDANDR-INP    PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000     03 MOD-RAD              OCCURS 13 TIMES.                             
008100*                                 TABELL INNEHÅLLANDE RADER.              
008200        05 MOD-KDKOLLI       PIC X(8).                                    
008300*                                 KOLLIKOD                                
008400        05 MOD-FILLER        PIC X(3).                                    
008500        05 MOD-KDEMBTYP      PIC 9.                                       
008600*                                 EMBALLAGETYP                            
008700        05 MOD-FILLER        PIC X(2).                                    
008800        05 MOD-DIKOLLIL      PIC Z(3)9.                                   
008900*                                 KOLLI-LÄNGD                             
009000        05 MOD-FILLER        PIC X(3).                                    
009100        05 MOD-DIKOLLIB      PIC Z(2)9.                                   
009200*                                 KOLLI-BREDD                             
009300        05 MOD-FILLER        PIC X(2).                                    
009400        05 MOD-DIKOLLIH      PIC Z(2)9.                                   
009500*                                 KOLLI-HÖJD                              
009600        05 MOD-FILLER        PIC X(3).                                    
009700        05 MOD-VKTARA        PIC Z(3)9.9.                                 
009800*                                 TARAVIKT (HG)                           
009900        05 MOD-FILLER        PIC X(2).                                    
010000        05 MOD-KDKOLLID      PIC X.                                       
010100*                                 KOLLI-DJUP                              
010200        05 MOD-FILLER        PIC X(4).                                    
010300        05 MOD-KVPALLAR      PIC Z9.                                      
010400*                                 ANTAL PALLAR         KVPALL-003         
010500        05 MOD-FILLER        PIC X(2).                                    
010600        05 MOD-KVRAM         PIC Z(2)9.                                   
010700*                                 ANTAL RAMAR                             
010800        05 MOD-FILLER        PIC X(2).                                    
010900        05 MOD-KVEMBSPA      PIC 9.                                       
011000*                                 ANTAL SPACE-EMBALLAGE                   
011100        05 MOD-FILLER        PIC X.                                       
011200        05 MOD-KVLOCK        PIC Z(2)9.                                   
011300*                                 ANTAL LOCK                              
011400        05 MOD-FILLER        PIC X(4).                                    
011500        05 MOD-KVINTPALL     PIC Z(2)9.                                   
011600*                                 ANTAL INTERNPALLAR   KVINTPALL          
011700        05 MOD-FILLER        PIC X(3).                                    
011800        05 MOD-EMBPROF       PIC X.                                       
011900*                                 EMBALLAGE PROFORMA-MÄRKNING             
012000     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
012100*                                 MEDDELANDEFÄLT PÅ RAD 23                
012200*** END OF VILMAII-COPY LENGTH= 1091 BYTES                                
