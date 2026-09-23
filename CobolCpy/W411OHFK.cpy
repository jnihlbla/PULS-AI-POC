000100 01  OHFK-W411OHFK.                                                       
000200*                                 LÄNKAREA TILL W411OHFK - FORMEL         
000300*                                 L KONTROLL AV ORDERHUVUDET              
000400     03 OHFK-IDSYSTEM        PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 OHFK-IDANALYS        PIC X(12).                                   
000700*                                 ANALYSNUMMER                            
000800     03 OHFK-IDANALYS-OK     PIC X.                                       
000900     03 OHFK-IDDISTR         PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 OHFK-IDDISTR-OK      PIC X.                                       
001200     03 OHFK-IDKUNDNR        PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 OHFK-IDKUNDNR-OK     PIC X.                                       
001500     03 OHFK-IDORDNR         PIC X(5).                                    
001600*                                 ORDERNUMMER                             
001700     03 OHFK-IDORDNR-OK      PIC X.                                       
001800     03 OHFK-KDORDKL         PIC X.                                       
001900*                                 ORDERKLASS                              
002000     03 OHFK-KDORDKL-OK      PIC X.                                       
002100     03 OHFK-KDFRAKT         PIC X(2).                                    
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300     03 OHFK-KDFRAKT-OK      PIC X.                                       
002400     03 OHFK-KDPROTYP        PIC X.                                       
002500*                                 TYP AV PROFORMA                         
002600     03 OHFK-KDPROTYP-OK     PIC X.                                       
002700     03 OHFK-FLAUTFAK        PIC X.                                       
002800*                                 AUTOMATFAKTURERING ?                    
002900     03 OHFK-FLAUTFAK-OK     PIC X.                                       
003000     03 OHFK-FLFORBI         PIC X.                                       
003100*                                 FÖRBIORDERFLAGGA                        
003200     03 OHFK-FLAUTPAC        PIC X.                                       
003300*                                 AUTOMATISK PACKRAPPORTERING             
003400     03 OHFK-FLAUTPAC-OK     PIC X.                                       
003500     03 OHFK-FLLSBOK         PIC X.                                       
003600*                                 LAGERAVBOKNING                          
003700     03 OHFK-FLLSBOK-OK      PIC X.                                       
003800     03 OHFK-FLORDSPE        PIC X.                                       
003900*                                 SPECIALORDERFLAGGA                      
004000     03 OHFK-FLVORKO         PIC X.                                       
004100*                                 VOR-KÖ FLAGGA                           
004200     03 OHFK-FLRESTN         PIC X.                                       
004300*                                 RESTNOTERING ?                          
004400     03 OHFK-FLRESTN-OK      PIC X.                                       
004500     03 OHFK-IDBIPREF        PIC X(7).                                    
004600*                                 BIPACKNINGSREFERENS                     
004700     03 OHFK-IDBIPREF-OK     PIC X.                                       
004800     03 OHFK-IDSKYLT         PIC X(3).                                    
004900      88 OHFK-GODK-IDSKYLT   VALUE 'CZ '                                  
005000                             'D  '                                        
005100                             'DK '                                        
005200                             'E  '                                        
005300                             'FB '                                        
005400                             'GB '                                        
005500                             'GR '                                        
005600                             'H  '                                        
005700                             'I  '                                        
005800                             'IR '                                        
005900                             'J  '                                        
006000                             'KOR'                                        
006100                             'MAL'                                        
006200                             'NL '                                        
006300                             'P  '                                        
006400                             'PL '                                        
006500                             'RC '                                        
006600                             'RCN'                                        
006700                             'RO '                                        
006800                             'RUS'                                        
006900                             'S  '                                        
007000                             'SF '                                        
007100                             'T  '                                        
007200                             'TR '                                        
007300                             'USA'                                        
007400                             'YU '.                                       
007500*                                 NATIONALITETSTECKEN                     
007600*                                 SPRÅKIDENTIFIKATION                     
007700     03 OHFK-IDSKYLT-OK      PIC X.                                       
007800     03 OHFK-IDFTG           PIC X(2).                                    
007900*                                 FÖRETAGSID EKONOM REDOVISNING           
008000     03 OHFK-IDFTG-OK        PIC X.                                       
008100     03 OHFK-IDKONTO         PIC X(10).                                   
008200*                                 KONTO                                   
008300     03 OHFK-IDKONTO-OK      PIC X.                                       
008400     03 OHFK-IDKST           PIC X(10).                                   
008500*                                 KOSTNADSSTÄLLE                          
008600     03 OHFK-IDKST-OK        PIC X.                                       
008700     03 OHFK-IDKAMPRF        PIC X(7).                                    
008800*                                 KAMPANJREFERENS                         
008900     03 OHFK-IDKAMPRF-OK     PIC X.                                       
009000     03 OHFK-IDDC            PIC X(2).                                    
009100*                                 IDENTIFIERARE LAGER                     
009200     03 OHFK-IDDC-OK         PIC X.                                       
009300     03 OHFK-KDFAKTYP        PIC X.                                       
009400*                                 FAKTURATYP                              
009500     03 OHFK-KDFAKTYP-OK     PIC X.                                       
009600     03 OHFK-KDROPACK        PIC X.                                       
009700*                                 FRISLÄPPNINGSKOD RO/DO                  
009800     03 OHFK-KDROPACK-OK     PIC X.                                       
009900     03 OHFK-KDTPOTYP        PIC X.                                       
010000*                                 TYP AV TIDPLANERAD ORDER                
010100     03 OHFK-KDTPOTYP-OK     PIC X.                                       
010200     03 OHFK-KDTULLVE        PIC X.                                       
010300*                                 TYP AV PRIS PÅ TULLFAKTURA              
010400     03 OHFK-KDTULLVE-OK     PIC X.                                       
010500     03 OHFK-KDVRINFO        PIC X.                                       
010600*                                 PÅVERKAN I VR/DSP SYSTEM                
010700     03 OHFK-KDVRINFO-OK     PIC X.                                       
010800     03 OHFK-TIFORDAT        PIC X(6).                                    
010900*                                 FÖRFALLODATUM                           
011000     03 OHFK-TIFORDAT-OK     PIC X.                                       
011100     03 OHFK-TITPO           PIC X(6).                                    
011200*                                 PLANERAD ORDERDATUM                     
011300     03 OHFK-TITPO-OK        PIC X.                                       
011400     03 OHFK-TIREGDAT        PIC X(6).                                    
011500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011600     03 OHFK-TIHHMM          PIC X(4).                                    
011700*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
011800     03 OHFK-TIRFSDAT        PIC X(6).                                    
011900*                                 KLART FÖR TRANSPORT ÅÅMMDD              
012000     03 OHFK-TIRFSDAT-OK     PIC X.                                       
012100     03 OHFK-TIRFSTID        PIC X(4).                                    
012200*                                 KLART FÖR TRANSPORT (TTMM)              
012300     03 OHFK-TIRFSTID-OK     PIC X.                                       
012400*** END OF VILMAII-COPY LENGTH= 147 BYTES                                 
