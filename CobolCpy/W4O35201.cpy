000100 01  MOD-W4O35201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 4352              
000300*                                 ORDERKÖ I PRODUKTIONSKANAL              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPRC-IN         PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDPRC-UT.                                                     
001100*                                 PRODUKTIONSKANAL                        
001200        05 MOD-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BAS                                 
001400        05 MOD-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MOD-IDUSER-IN        PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDUSER-UT        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 MOD-IDBORD-IN        PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDBORD-UT        PIC X(3).                                    
002300*                                 PACK-BORD                               
002400     03 MOD-IDTRP-IN         PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDTRP-UT.                                                     
002700*                                 TRANSPORTIDENTITET                      
002800        05 MOD-IDTRPLOS      PIC X(3).                                    
002900*                                 TRANSPORTLÖSNING                        
003000        05 MOD-IDTRPVAR      PIC X(2).                                    
003100*                                 TRANSPORTLÖSNINGSGRUPP                  
003200     03 MOD-IDDC-IN          PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400     03 MOD-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-KVORDER-ENTER    PIC X(7).                                    
003700*                                 ANTAL ORDER                             
003800     03 MOD-KVRADER-ENTER    PIC X(5).                                    
003900*                                 ANTAL RADER                             
004000     03 MOD-IDTRP-ENTER.                                                  
004100*                                 TRANSPORTIDENTITET                      
004200        05 MOD-IDTRPLOS      PIC X(3).                                    
004300*                                 TRANSPORTLÖSNING                        
004400        05 MOD-IDTRPVAR      PIC X(2).                                    
004500*                                 TRANSPORTLÖSNINGSGRUPP                  
004600     03 MOD-TIAAMMDD-ENTER   PIC 9(6).                                    
004700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004800     03 MOD-TIHHMM-ENTER     PIC X(5).                                    
004900*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005000     03 MOD-TIRFS-ENTER      PIC 9(10).                                   
005100*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
005200     03 MOD-TIUTSKR-ENTER    PIC 9(6).                                    
005300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
005400     03 MOD-KVORDER-NEXT     PIC X(7).                                    
005500*                                 ANTAL ORDER                             
005600     03 MOD-KVRADER-NEXT     PIC X(5).                                    
005700*                                 ANTAL RADER                             
005800     03 MOD-IDPLKLST-NEXT    PIC X(3).                                    
005900*                                 PLOCKLISTNUMMER                         
006000     03 MOD-IDPLKLST-ENTER   PIC X(3).                                    
006100*                                 PLOCKLISTNUMMER                         
006200     03 MOD-IDPRC-SPAR.                                                   
006300*                                 PRODUKTIONSKANAL                        
006400        05 MOD-IDPRCBAS      PIC X(3).                                    
006500*                                 PRC-BAS                                 
006600        05 MOD-IDPRCVAR      PIC X.                                       
006700*                                 PRC-VARIANT                             
006800     03 MOD-TILST-O-ENTER    PIC 9(10).                                   
006900*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
007000     03 MOD-TABELLRAD        OCCURS 11 TIMES.                             
007100*                                 GRUPP MED TABELL RADER                  
007200        05 MOD-FLORDDEL-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-FLORDDEL      PIC X.                                       
007500*                                 ALLMÄN FLAGGA                           
007600        05 MOD-IDPRCVAR-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-IDPRCVAR-RAD  PIC X.                                       
007900*                                 PRC-VARIANT                             
008000        05 MOD-FLIDUSER-ATTR PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-FLIDUSER      PIC X.                                       
008300*                                 ALLMÄN FLAGGA                           
008400        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-IDDISTR       PIC Z(3)9.                                   
008700*                                 DISTRIKTNUMMER                          
008800        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
009100*                                 KUNDNUMMER                              
009200        05 MOD-FLFLER        PIC X.                                       
009300*                                 ALLMÄN FLAGGA                           
009400        05 MOD-IDORDNR5-ATTR PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-IDORDNR5      PIC X(5).                                    
009700*                                 ORDERNUMMER                             
009800        05 MOD-BEODEINF-ATTR PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 MOD-BEODEINF      PIC X(54).                                   
010100*                                 ORDERDELSINFORMATION    BEODEIN         
010200*                                 F                                       
010300     03 MOD-ORDERDELSNYCKEL  OCCURS 12 TIMES.                             
010400*                                 NYCKEL TILL ORDERDEL                    
010500        05 MOD-IDPRODNR-KEY  PIC X(7).                                    
010600*                                 PRODUKTIONSNUMMER                       
010700        05 MOD-IDPLKLST-KEY  PIC X(3).                                    
010800*                                 PLOCKLISTNUMMER                         
010900     03 MOD-TIUTSTID-ENTER   PIC 9(6).                                    
011000*                                 UTSKRIFTSTID (TTMMSS)                   
011100     03 MOD-IDTRP-NEXT.                                                   
011200*                                 TRANSPORTIDENTITET                      
011300        05 MOD-IDTRPLOS      PIC X(3).                                    
011400*                                 TRANSPORTLÖSNING                        
011500        05 MOD-IDTRPVAR      PIC X(2).                                    
011600*                                 TRANSPORTLÖSNINGSGRUPP                  
011700     03 MOD-TIAAMMDD-NEXT    PIC 9(6).                                    
011800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
011900     03 MOD-TIHHMM-NEXT      PIC X(5).                                    
012000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
012100     03 MOD-TIRFS-NEXT       PIC 9(10).                                   
012200*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
012300     03 MOD-TILST-O-NEXT     PIC 9(10).                                   
012400*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
012500     03 MOD-TIUTSKR-NEXT     PIC 9(6).                                    
012600*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
012700     03 MOD-TIUTSTID-NEXT    PIC 9(6).                                    
012800*                                 UTSKRIFTSTID (TTMMSS)                   
012900     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-FLKLAR           PIC X.                                       
013200*                                 AVSLUTNINGSMARKERING                    
013300     03 MOD-KDPRT-PU-ATTR    PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500     03 MOD-KDPRT-PU         PIC X(3).                                    
013600*                                 PRINTERKOD PACKUNDERLAG                 
013700     03 MOD-KDPRT-PLE-ATTR   PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900     03 MOD-KDPRT-PLE        PIC X(3).                                    
014000*                                 PRINTERKOD PLOCKETIKETTER               
014100     03 MOD-TEMFSINF         PIC X(55).                                   
014200*                                 INFORMATIONSMEDDELANDE                  
014300*** END OF VILMAII-COPY LENGTH= 1351 BYTES                                
