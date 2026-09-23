000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4050400.                                                
000400 AUTHOR.         STEFANO GIOBBI.                                          
000500 DATE-WRITTEN.   MARS. 90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        PROGRAMMET ÄR ETT FRÅGEPROGRAM.                                  
001200*                                                                         
001300*        PROGRAMMET VISAR, PER DISTRIKT/KUND/ORDER/DC,                    
001400*        ORDER- OCH KUNDINFORMATION.                                      
001500*        KONTROLL GÖRS ATT ORDERHUVUDET HAR ORDERRADER.                   
001600*                                                                         
001700*        PROGRAMMET STARTAR OCKSÅ PÅ BEGÄRAN BAKGR-MPP W402950            
001800*                                                                         
001900*        BLÄDDRING FÖREKOMMER EJ.                                         
002000*                                                                         
002100*    TRANSAKTION: W4T504                                                  
002200*    MID:         W4I50401                                                
002300*                 W4I29501                                                
002400*    MOD:         W4O50401                                                
002500*                                                                         
002600*    BASER:                                                               
002700*        FYSISKT  LOGISKT   COPYTEXT    PREFIX (COPYTEXT)                 
002800*                                                                         
002900*        WDQ201   WLORQI01  WDQ201      OHUV-                             
003000*            12         12      12      ARB-                              
003100*            C1         13      C1      SEQC-                             
003200*                                                                         
003300*        WDB201   WDB201                GMT-                              
003400*                                                                         
003500*        WDB101   WDB101    WDB101      BET-                              
003600*                                                                         
003700*        WDB601             WDB601      DCS-                              
003800     EJECT                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP3                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400*    -- CHECKED BY WY2000                                                 
004500     SKIP3                                                                
004600*                                                                         
004700 77  IDPGM                   PIC X(8)    VALUE 'W4050400'.                
004800 77  JA                      PIC X       VALUE 'J'.                       
004900 77  NEJ                     PIC X       VALUE 'N'.                       
005000 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
005100                                                                          
005200 77  WS-IDTIDZON             PIC X(2)    VALUE SPACE.                     
005300                                                                          
005400 77  IND-X                   PIC S9(3)   VALUE ZERO COMP-3.               
005500                                                                          
005600 01  W-SPAR-IDKUNDRF.                                                     
005700     03  W-SPAR-IDORDNR7     PIC X(7)    VALUE '+++++++'.                 
005800     03  FILLER              PIC X(3)    VALUE '+++'.                     
005900                                                                          
006000                                                                          
006100 77  DAGENS-DATUM            PIC 9(6).                                    
006200 77  DAGENS-TID              PIC 9(8).                                    
006300                                                                          
006400 77  INDATA-SW               PIC X       VALUE 'J'.                       
006500   88  INDATA-OK                         VALUE 'J'.                       
006600   88  INDATA-FEL                        VALUE 'N'.                       
006700                                                                          
006800 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
006900   88  NYCKLAR-OK                        VALUE 'J'.                       
007000   88  NYCKLAR-FEL                       VALUE 'N'.                       
007100 77  ALLT-SW                 PIC X       VALUE 'J'.                       
007200   88  ALLT-OK                           VALUE 'J'.                       
007300     EJECT                                                                
007400 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
007500   88  EGEN-MID                          VALUE '4504'.                    
007600   88  GODK-MID                          VALUE '4501' '4506'              
007700                                               '4502' '4507'              
007800                                               '4503' '4508'              
007900                                               '4504' '4509'              
008000                                               '4505' '4510'.             
008100     SKIP2                                                                
008200 01  TEST-MED1.                                                           
008300     03  TEST-KDMFSFOR       PIC X.                                       
008400     03  TEST-IDDISTR        PIC 9(4).                                    
008500     03  TEST-IDKUNDNR       PIC 9(6).                                    
008600     03  TEST-IDKUNDRF       PIC X(10).                                   
008700     03  TEST-IDSID          PIC 9(3).                                    
008800     03  TEST-IDSYSTEM       PIC X(4).                                    
008900     03  TEST-IDPRT          PIC X(3).                                    
009000 01  TEST-MED2.                                                           
009100     03  TEST-TIUPPDAT       PIC 9(6).                                    
009200     03  TEST-TIUPPTID       PIC 9(8).                                    
009300     03  TEST-NYCKEL         PIC X(19).                                   
009400*                                                                         
009500*      --- VALID IDDC CODES                                               
009600*                                                                         
009700*01    -COPY WWDCKONS                                                     
009800       EJECT                                                              
009900 01  GENERELLA-SUBPROGRAM.                                                
010000*                                                                         
010100   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
010200   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
010300   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
010400   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
010500   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010800*01 -COPY WMSGINIT                                                        
010900     EJECT                                                                
011000 01  KONSTANT-AREOR.                                                      
011100*                                                                         
011200   03  K-DC-SAKNAS-026        PIC X(3)              VALUE '026'.          
011300   03  K-ORDERN-ANNULLERAD-052                                            
011400                              PIC X(3)              VALUE '052'.          
011500   03  K-UTSKRIFT-BEGAERD-118 PIC X(3)              VALUE '118'.          
011600   03  K-NYCKEL-FEL-401       PIC X(3)              VALUE '401'.          
011700   03  K-ORDERINFO-BORTTAGEN-415                                          
011800                              PIC X(3)              VALUE '415'.          
011900   03  K-ORDER-SAKNAS-701     PIC X(3)              VALUE '701'.          
012000   03  K-FELAKTIG-PRINTER-772 PIC X(3)              VALUE '772'.          
012100     SKIP2                                                                
012200 01  HELP-AREOR.                                                          
012300*                                                                         
012400   03  HELP-TITRPAVT-AAMMDD  PIC  9(6)                VALUE ZERO.         
012500   03  HELP-TITRPAVT-HHMM    PIC  9(4)                VALUE ZERO.         
012600   03  HELP-TIHHMM-TIM-MIN REDEFINES HELP-TITRPAVT-HHMM.                  
012700       05  HELP-TIHHMM-TIM                                                
012800                             PIC  9(2).                                   
012900       05  HELP-TIHHMM-MIN                                                
013000                             PIC  9(2).                                   
013100   03  HELP-TIHHMM-REDIGERAD.                                             
013200       05  HELP-TIHHMM-TIM-RED                                            
013300                             PIC  X(2)                VALUE SPACE.        
013400       05  HELP-TIHHMM-PUNKT PIC  X(1)                VALUE '.'.          
013500       05  HELP-TIHHMM-MIN-RED                                            
013600                             PIC  X(2)                VALUE SPACE.        
013700   03  HELP-TIRFS            PIC  9(10)               VALUE ZERO.         
013800     EJECT                                                                
013900*   -COPY WMEDAREA                                                        
014000     EJECT                                                                
014100******************************************************************        
014200*                                                                *        
014300*                AREOR FÖR ANROP AV W006PRT                      *        
014400*                                                                *        
014500******************************************************************        
014600*   -COPY W006PRT                                                         
014700     EJECT                                                                
014800******************************************************************        
014900*                                                                *        
015000*                AREOR FÖR ANROP AV W4029500                     *        
015100*                                                                *        
015200******************************************************************        
015300 01  P-TO-P-SW.                                                           
015400     03  PTOP-LL             PIC S9(4)   VALUE +92 COMP SYNC.             
015500     03  PTOP-Z1             PIC X       VALUE LOW-VALUE.                 
015600     03  PTOP-Z2             PIC X       VALUE LOW-VALUE.                 
015700     03  PTOP-TRANSKOD       PIC X(7)    VALUE 'W4T295X'.                 
015800     03  FILLER              PIC X       VALUE SPACE.                     
015900     03  PTOP-IDTRANS        PIC X(4)    VALUE '4504'.                    
016000     03  PTOP-KDMFSFOR       PIC X.                                       
016100*    03  MID -COPY W4I29501                                               
016200     EJECT                                                                
016300******************************************************************        
016400*                                                                *        
016500*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
016600*                                                                *        
016700******************************************************************        
016800 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
016900     SKIP3                                                                
017000*01  MID -COPY W4I50401                                                   
017100     EJECT                                                                
017200*01  -COPY WMSGAREA                                                       
017300     EJECT                                                                
017400*  03  MOD -COPY W4O50401 -RED MSG-AREA.                                  
017500     EJECT                                                                
017600*01  -COPY WMFSAREA                                                       
017700     EJECT                                                                
017800******************************************************************        
017900*                                                                *        
018000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                       *        
018100*                                                                *        
018200******************************************************************        
018300*                                                                         
018400 01  IMS-WS.                                                              
018500   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
018600     SKIP3                                                                
018700 01  NYCKLAR-TILL-DLI.                                                    
018800*                                                                         
018900   03  W-IDORDER-X.                                                       
019000       05  W-WDQ2-IDORDER      PIC S9(7) COMP-3 VALUE +0.                 
019100*                                                                         
019200   03  W-WDQ2CSEQ-X.                                                      
019300       05  W-WDQ2CSEQ-IDGMTREF.                                           
019400           07  W-WDQ2CSEQ-IDDISTR                                         
019500                               PIC S9(5) COMP-3 VALUE +0.                 
019600           07  W-WDQ2CSEQ-IDKUNDNR                                        
019700                               PIC S9(7) COMP-3 VALUE +0.                 
019800           07  W-WDQ2CSEQ-IDKUNDRF                                        
019900                               PIC X(10)        VALUE SPACE.              
020000*                                                                         
020100   03  W-IDDC-X.                                                          
020200       05  W-WDQ2-IDDC         PIC X(2)         VALUE SPACE.              
020300*                                                                         
020400   03  W-WDB201KEY-X.                                                     
020500       05  W-WDB2-IDDISTR      PIC S9(5) COMP-3 VALUE +0.                 
020600       05  W-WDB2-IDKUNDNR     PIC S9(7) COMP-3 VALUE +0.                 
020700                                                                          
020800   03  W-WDB101KY-X.                                                      
020900       05  W-WDB1-IDPARTNR     PIC X(9)         VALUE SPACE.              
021000       05  W-WDB1-IDFTG        PIC 9(2)         VALUE ZERO.               
021100                                                                          
021200   03  W-IDDC-B6-X.                                                       
021300       05 W-IDDC-B6            PIC X(2).                                  
021400     EJECT                                                                
021500******************************************************************        
021600*                                                                *        
021700*        STATUSKODER FRÅN IMS                                    *        
021800*                                                                *        
021900******************************************************************        
022000*                                                                         
022100   03  STATUS-WS             PIC X(2).                                    
022200*                                                                         
022300     88  SEGMENT-FINNS                   VALUE '  '.                      
022400     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
022500     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
022600     SKIP3                                                                
022700   03  GODK-STATUSKODER.                                                  
022800     05  GODK-STATUS OCCURS  5                                            
022900                     INDEXED BY STATUS-IX PIC X(2).                       
023000     SKIP3                                                                
023100******************************************************************        
023200*                                                                *        
023300*        SSA:ER                                                  *        
023400*                                                                *        
023500******************************************************************        
023600*                                                                         
023700 01    SSA1                  PIC X(128).                                  
023800 01    SSA2                  PIC X(128).                                  
023900     EJECT                                                                
024000******************************************************************        
024100*                                                                *        
024200*        IMS FUNKTIONSKODER                                      *        
024300*                                                                *        
024400******************************************************************        
024500*                                                                         
024600*01    -COPY W0003                                                        
024700     EJECT                                                                
024800******************************************************************        
024900*                                                                *        
025000*        ARBETS-AREOR TILL IO-AREORNA                                     
025100*                                                                *        
025200*        DLI INPUT-OUTPUT AREA                                   *        
025300*                                                                *        
025400******************************************************************        
025500*    ---  DLI INPUT-OUTPUT                                                
025600*    ---  DLI-IO-AREA                                                     
025700*                                                                         
025800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI01'.           
025900 01  DLI-IO-WLORQI01.                                                     
026000*  03  WLORQI01  -COPY WDQ201                                             
026100     EJECT                                                                
026200                                                                          
026300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQI12'.           
026400 01  DLI-IO-WLORQI12.                                                     
026500*  03  WLORQI12  -COPY WDQ212                                             
026600     EJECT                                                                
026700                                                                          
026800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-GMTA01'.           
026900 01  DLI-IO-WLGMTA01.                                                     
027000*  03  WLKNDB01  -COPY WDB201                                             
027100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDB101'.           
027200 01  DLI-IO-WDB101.                                                       
027300*  03  WLKNDB01  -COPY WDB101                                             
027400                                                                          
027500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
027600 01   DLI-IO-AREA-B601.                                                   
027700*     03  -COPY WDB601                                                    
027800*                                                                         
027900 LINKAGE SECTION.                                                         
028000                                                                          
028100*01  -COPY W0009     -PRE MSG-                                            
028200     EJECT                                                                
028300*01  -COPY W0009     -PRE ALT-                                            
028400     EJECT                                                                
028500*01  -COPY W0008     -PRE USEA-                                           
028600     05  FILLER              PIC X(35).                                   
028700     EJECT                                                                
028800*01  -COPY W0008     -PRE ORQI-                                           
028900     05  FILLER              PIC X(35).                                   
029000     EJECT                                                                
029100*01  -COPY W0008     -PRE GMTA-                                           
029200     05  FILLER              PIC X(35).                                   
029300     EJECT                                                                
029400*01  -COPY W0008     -PRE WDB1-                                           
029500     05  FILLER              PIC X(35).                                   
029600     EJECT                                                                
029700*01  -COPY W0008     -PRE WDB6-                                           
029800     05  FILLER                  PIC X.                                   
029900 PROCEDURE DIVISION  USING MSG-PCB                                        
030000                           ALT-PCB                                        
030100                           USEA-PCB                                       
030200                           ORQI-PCB                                       
030300                           GMTA-PCB                                       
030400                           WDB1-PCB                                       
030500                           WDB6-PCB.                                      
030600                                                                          
030700     ENTRY 'DLITCBL' USING MSG-PCB                                        
030800                           ALT-PCB                                        
030900                           USEA-PCB                                       
031000                           ORQI-PCB                                       
031100                           GMTA-PCB                                       
031200                           WDB1-PCB                                       
031300                           WDB6-PCB.                                      
031400                                                                          
031500     PERFORM IMS-GET-MSG                                                  
031600     IF SEGMENT-FINNS                                                     
031700       PERFORM A-INIT                                                     
031800       PERFORM B-KOLLA-NYCKLAR                                            
031900       IF NYCKLAR-OK                                                      
032000         IF MFS-ENTER OR                                                  
032100            MFS-FIRST OR                                                  
032200            MFS-NEXT  OR                                                  
032300            MFS-PRINT                                                     
032400           PERFORM C-LAES-BASER-REDIGERA-BILD                             
032500         END-IF                                                           
032600         IF MFS-PRINT AND                                                 
032700            MOD-TEMFSFEL = MFS-RENSA-FAELT                                
032800           PERFORM D-STARTA-UTSKRIFT                                      
032900         END-IF                                                           
033000       END-IF                                                             
033100       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O50401 + 4                      
033200       PERFORM IMS-INSERT-MSG                                             
033300     END-IF                                                               
033400                                                                          
033500     MOVE ZERO TO RETURN-CODE                                             
033600     GOBACK                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 A-INIT SECTION.                                                          
034000                                                                          
034100     IF MSG-DUBBLA-TRANSKODER                                             
034200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I50401                 
034300       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
034400                                               W-IDTRANS                  
034500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
034600     ELSE                                                                 
034700       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I50401                 
034800       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
034900                                               W-IDTRANS                  
035000       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
035100     END-IF                                                               
035200                                                                          
035300     IF EGEN-MID                                                          
035400       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
035500       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
035600     ELSE                                                                 
035700       MOVE SPACE                         TO MFS-KDTRTYP                  
035800       MOVE '7'                           TO MFS-IDPFK                    
035900     END-IF                                                               
036000                                                                          
036100     MOVE LOW-VALUE                       TO MSG-AREA                     
036200     MOVE 'W4O504N1'                      TO MFS-IDMOD                    
036300     MOVE '4504'                          TO MOD-IDTRANS                  
036400     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
036500                                             MOD-TEMFSINF                 
036600                                                                          
036700     ACCEPT DAGENS-DATUM FROM DATE                                        
036800     ACCEPT DAGENS-TID   FROM TIME                                        
036900     .                                                                    
037000     EJECT                                                                
037100 B-KOLLA-NYCKLAR SECTION.                                                 
037200                                                                          
037300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037400     MOVE '001'             TO MSGI-KDCALL                                
037500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037600     MOVE '4504'            TO MSGI-IDTRANS                               
037700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037800     IF EGEN-MID                                                          
037900        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
038000        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
038100        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
038200        IF MID-IDKUNDRF-IN      NOT = ALL '+'                             
038300           MOVE MID-IDKUNDRF-IN TO W-SPAR-IDORDNR7                        
038400           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
038500        END-IF                                                            
038600        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
038700        MOVE MID-IDKOLLI-IN     TO MSGI-IDKOLLI                           
038800     END-IF                                                               
038900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039000     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
039100     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
039200                                                                          
039300     MOVE      JA               TO        NYCKLAR-SW                      
039400     MOVE      MFS-RENSA-FAELT  TO        MOD-IDDISTR-IN                  
039500                                          MOD-IDKUNDNR-IN                 
039600                                          MOD-IDKUNDRF-IN                 
039700                                          MOD-IDDC-IN                     
039800                                          MOD-IDARTNR-IN                  
039900                                          MOD-IDKOLLI-IN                  
040000                                          MOD-IDPRODNR-IN                 
040100                                                                          
040200*    MOVE    '7'                TO        MFS-IDPFK                       
040300     MOVE    SPACE              TO        MFS-KDTRTYP                     
040400                                                                          
040500     PERFORM BA-KONTROLLERA-IDDISTR                                       
040600     PERFORM BB-KONTROLLERA-IDKUNDNR                                      
040700     PERFORM BC-KONTROLLERA-IDKUNDRF                                      
040800     PERFORM BD-KONTROLLERA-IDDC                                          
040900     PERFORM BE-KONTROLLERA-IDARTNR                                       
041000     PERFORM BF-KONTROLLERA-IDKOLLI                                       
041100     PERFORM BG-KONTROLLERA-IDPRODNR                                      
041200                                                                          
041300     IF NYCKLAR-FEL AND                                                   
041400        NOT GODK-MID                                                      
041500       MOVE  MFS-RENSA-FAELT    TO        MOD-IDDISTR-UT                  
041600                                          MOD-IDKUNDNR-UT                 
041700                                          MOD-IDKUNDRF-UT                 
041800                                          MOD-IDARTNR-UT                  
041900                                          MOD-IDKOLLI-UT                  
042000                                          MOD-IDPRODNR-UT                 
042100     END-IF                                                               
042200                                                                          
042300     IF GODK-MID                                                          
042400        CONTINUE                                                          
042500     ELSE                                                                 
042600       MOVE MFS-RENSA-FAELT      TO        MOD-IDARTNR-UT                 
042700                                           MOD-IDKOLLI-UT                 
042800                                           MOD-IDPRODNR-UT                
042900     END-IF                                                               
043000                                                                          
043100     IF NYCKLAR-FEL                                                       
043200       MOVE    K-NYCKEL-FEL-401 TO        MED-IDMFSFEL                    
043300       CALL    WMEDKONV         USING     MED-WMEDAREA                    
043400       MOVE    MED-MFSFEL       TO        MOD-TEMFSFEL                    
043500       PERFORM MFS-RENSA-FAELT-UT                                         
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900 BA-KONTROLLERA-IDDISTR SECTION.                                          
044000                                                                          
044100     MOVE      MSGI-IDDISTR    TO        MOD-IDDISTR-UT                   
044200     INSPECT   MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
044300                                                                          
044400     IF MSGI-IDDISTR  NUMERIC AND MSGI-IDDISTR > ZERO                     
044500       MOVE    MSGI-IDDISTR    TO        W-WDQ2CSEQ-IDDISTR               
044600                                         W-WDB2-IDDISTR                   
044700     ELSE                                                                 
044800       MOVE    NEJ             TO        NYCKLAR-SW                       
044900     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200 BB-KONTROLLERA-IDKUNDNR SECTION.                                         
045300                                                                          
045400     MOVE      MSGI-IDKUNDNR    TO        MOD-IDKUNDNR-UT                 
045500     INSPECT   MOD-IDKUNDNR-UT  REPLACING LEADING ZERO BY SPACE.          
045600                                                                          
045700     IF MSGI-IDKUNDNR NUMERIC                                             
045800       MOVE    MSGI-IDKUNDNR    TO        W-WDQ2CSEQ-IDKUNDNR             
045900                                          W-WDB2-IDKUNDNR                 
046000     ELSE                                                                 
046100       MOVE    NEJ              TO        NYCKLAR-SW                      
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 BC-KONTROLLERA-IDKUNDRF SECTION.                                         
046600                                                                          
046700     MOVE      MSGI-IDKUNDRF(1:7)  TO        MOD-IDKUNDRF-UT              
046800     INSPECT   MOD-IDKUNDRF-UT  REPLACING LEADING ZERO BY SPACE           
046900                                                                          
047000     IF MSGI-IDKUNDRF(1:7)      NUMERIC AND                               
047100        MSGI-IDKUNDRF(1:7)      > ZERO                                    
047200       MOVE    MSGI-IDKUNDRF(1:7) TO        W-WDQ2CSEQ-IDKUNDRF           
047300     ELSE                                                                 
047400       MOVE    NEJ              TO        NYCKLAR-SW                      
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 BD-KONTROLLERA-IDDC SECTION.                                             
047900                                                                          
048000     IF EGEN-MID                                                          
048100        IF MID-IDDC-IN     = ALL '+'                                      
048200          MOVE MID-IDDC-UT TO W-IDDC-B6                                   
048300        ELSE                                                              
048400          MOVE MID-IDDC-IN TO W-IDDC-B6                                   
048500          MOVE '7'        TO MFS-IDPFK                                    
048600          MOVE SPACE      TO MFS-KDTRTYP                                  
048700        END-IF                                                            
048800     ELSE                                                                 
048900        MOVE MSGI-IDDC     TO W-IDDC-B6                                   
049000     END-IF                                                               
049100     PERFORM IMS-GU-WDB601                                                
049200                                                                          
049300     IF DCS-KDDC = SPACE                                                  
049400       MOVE    NEJ         TO NYCKLAR-SW                                  
049500     ELSE                                                                 
049600       IF DCS-DDC                                                         
049700         MOVE WC-CDC-SE    TO W-WDQ2-IDDC                                 
049800       ELSE                                                               
049900         MOVE DCS-IDDC     TO W-WDQ2-IDDC                                 
050000       END-IF                                                             
050100     END-IF                                                               
050200                                                                          
050300     MOVE W-IDDC-B6        TO MOD-IDDC-UT                                 
050400     .                                                                    
050500     EJECT                                                                
050600 BE-KONTROLLERA-IDARTNR SECTION.                                          
050700                                                                          
050800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
050900                                                                          
051000     IF MID-IDARTNR-IN = ALL '+'                                          
051100       MOVE MID-IDARTNR-UT TO MOD-IDARTNR-UT                              
051200     ELSE                                                                 
051300       MOVE MID-IDARTNR-IN TO MOD-IDARTNR-UT                              
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700 BF-KONTROLLERA-IDKOLLI SECTION.                                          
051800                                                                          
051900     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
052000                                                                          
052100     IF MID-IDKOLLI-IN = ALL '+'                                          
052200       MOVE MID-IDKOLLI-UT TO MOD-IDKOLLI-UT                              
052300     ELSE                                                                 
052400       MOVE MID-IDKOLLI-IN TO MOD-IDKOLLI-UT                              
052500     END-IF                                                               
052600     .                                                                    
052700     EJECT                                                                
052800 BG-KONTROLLERA-IDPRODNR SECTION.                                         
052900                                                                          
053000     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
053100                                                                          
053200     IF MID-IDPRODNR-IN = ALL '+'                                         
053300       MOVE MID-IDPRODNR-UT TO MOD-IDPRODNR-UT                            
053400     ELSE                                                                 
053500       MOVE MID-IDPRODNR-IN TO MOD-IDPRODNR-UT                            
053600     END-IF                                                               
053700     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
053800     .                                                                    
053900     EJECT                                                                
054000 C-LAES-BASER-REDIGERA-BILD SECTION.                                      
054100                                                                          
054200     PERFORM MFS-RENSA-FAELT-UT                                           
054300     PERFORM IMS-GU-ORQI01                                                
054400                                                                          
054500     IF SEGMENT-FINNS                                                     
054600       PERFORM CA-RED-BILD-M-OHUVINFO                                     
054700                                                                          
054800       IF OHUV-FLBORT = NEJ                                               
054900         PERFORM IMS-GNP-ORQI12-FIRST                                     
055000         IF SEGMENT-FINNS                                                 
055100           PERFORM CC-RED-BILD-M-ARBTAB-INFO                              
055200         ELSE                                                             
055300           PERFORM CD-RENSA-ARBTAB-FAELT                                  
055400         END-IF                                                           
055500                                                                          
055600         PERFORM IMS-GU-GMTA01                                            
055700         MOVE GMT-IDPARTNR   TO W-WDB1-IDPARTNR                           
055800         MOVE DCS-IDFTG      TO W-WDB1-IDFTG                              
055900                                                                          
056000         PERFORM IMS-GU-WDB101                                            
056100         PERFORM CE-RED-BILD-M-KUNDINFO                                   
056200                                                                          
056300         IF OHUV-KDTPOTYP > ZERO                                          
056400           PERFORM CF-BEH-ORDERRADER-SAKNAS-TPO                           
056500         END-IF                                                           
056600                                                                          
056700       ELSE                                                               
056800         PERFORM CG-BEH-ANNULLERAD-ORDER                                  
056900       END-IF                                                             
057000     ELSE                                                                 
057100       PERFORM CH-BEH-ORDER-SAKNAS                                        
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 CA-RED-BILD-M-OHUVINFO SECTION.                                          
057600                                                                          
057700     MOVE MID-IDPRT-IN       TO MOD-IDPRT-IN                              
057800     MOVE OHUV-BEKUNDRF      TO MOD-BEKUNDRF                              
057900     MOVE OHUV-TIREGDAT      TO MOD-TIREGDAT                              
058000     MOVE OHUV-IDKAMPRF      TO MOD-IDKAMPRF                              
058100     MOVE OHUV-BEBETRAD-1    TO MOD-BEBET-RAD1                            
058200     MOVE OHUV-BEBETRAD-2    TO MOD-BEBET-RAD2                            
058300     MOVE OHUV-ADBETRAD-1    TO MOD-ADBET-RAD1                            
058400     MOVE OHUV-ADBETRAD-2    TO MOD-ADBET-RAD2                            
058500     MOVE OHUV-BEGMT-RAD1       TO MOD-BEGMT-RAD1                         
058600     MOVE OHUV-BEGMT-RAD2       TO MOD-BEGMT-RAD2                         
058700     MOVE OHUV-ADGMT-GATA       TO MOD-ADGMT-RAD1                         
058800     MOVE OHUV-ADGMT-PADR       TO MOD-ADGMT-RAD2                         
058900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-IN-ATTR                      
059000     .                                                                    
059100     EJECT                                                                
059200 CC-RED-BILD-M-ARBTAB-INFO SECTION.                                       
059300                                                                          
059400     MOVE ARB-TIRFS             TO HELP-TIRFS                             
059500     MOVE HELP-TIRFS            TO MOD-TIAAMMDD-RFS                       
059600     MOVE ARB-IDTRP             TO MOD-IDTRP                              
059700                                                                          
059800     MOVE ARB-DATRPAVD (3:6)    TO HELP-TITRPAVT-AAMMDD                   
059900     MOVE HELP-TITRPAVT-AAMMDD  TO MOD-TIAAMMDD-TRP                       
060000                                                                          
060100     MOVE ARB-TIHHMM            TO HELP-TITRPAVT-HHMM                     
060200     MOVE HELP-TIHHMM-TIM       TO HELP-TIHHMM-TIM-RED                    
060300     MOVE HELP-TIHHMM-MIN       TO HELP-TIHHMM-MIN-RED                    
060400     MOVE HELP-TIHHMM-REDIGERAD TO MOD-TIHHMM                             
060500                                                                          
060600     MOVE ARB-BEGMRK          TO MOD-BEGDSMRK-GRP                         
060700     .                                                                    
060800     EJECT                                                                
060900 CD-RENSA-ARBTAB-FAELT SECTION.                                           
061000                                                                          
061100     MOVE MFS-RENSA-FAELT TO                                              
061200                                MOD-TIAAMMDD-RFS                          
061300                                MOD-IDTRP                                 
061400                                MOD-TIAAMMDD-TRP                          
061500                                MOD-TIHHMM                                
061600                                                                          
061700     MOVE K-DC-SAKNAS-026 TO    MED-IDMFSINF                              
061800     CALL WMEDKONV        USING MED-WMEDAREA                              
061900     MOVE MED-MFSINF      TO    MOD-TEMFSINF                              
062000     .                                                                    
062100     EJECT                                                                
062200 CE-RED-BILD-M-KUNDINFO SECTION.                                          
062300                                                                          
062400     IF SEGMENT-FINNS                                                     
062500       MOVE BET-BEBETRAD-1     TO MOD-BEKOPARE-RAD1                       
062600       MOVE BET-BEBETRAD-2     TO MOD-BEKOPARE-RAD2                       
062700       MOVE BET-ADBETRAD-1     TO MOD-ADKOPARE-RAD1                       
062800       MOVE BET-ADBETRAD-2     TO MOD-ADKOPARE-RAD2                       
062900     ELSE                                                                 
063000       MOVE SPACE              TO MOD-BEKOPARE                            
063100                                  MOD-ADKOPARE                            
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 CF-BEH-ORDERRADER-SAKNAS-TPO SECTION.                                    
063600                                                                          
063700     PERFORM MFS-RENSA-FAELT-UT                                           
063800     MOVE    K-ORDER-SAKNAS-701 TO    MED-IDMFSINF                        
063900     CALL    WMEDKONV           USING MED-WMEDAREA                        
064000     MOVE    MED-MFSINF         TO    MOD-TEMFSINF                        
064100     .                                                                    
064200     EJECT                                                                
064300 CG-BEH-ANNULLERAD-ORDER SECTION.                                         
064400                                                                          
064500     PERFORM MFS-RENSA-FAELT-UT                                           
064600     PERFORM IMS-GNP-ORQI12-FIRST                                         
064700     IF SEGMENT-FINNS                                                     
064800       MOVE  K-ORDERN-ANNULLERAD-052 TO    MED-IDMFSINF                   
064900       CALL  WMEDKONV                USING MED-WMEDAREA                   
065000       MOVE  MED-MFSINF              TO    MOD-TEMFSINF                   
065100     ELSE                                                                 
065200       MOVE  K-ORDERINFO-BORTTAGEN-415 TO    MED-IDMFSINF                 
065300       CALL  WMEDKONV                  USING MED-WMEDAREA                 
065400       MOVE  MED-MFSINF                TO    MOD-TEMFSINF                 
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
065800 CH-BEH-ORDER-SAKNAS SECTION.                                             
065900                                                                          
066000     PERFORM MFS-RENSA-FAELT-UT                                           
066100                                                                          
066200     MOVE    K-ORDER-SAKNAS-701 TO    MED-IDMFSINF                        
066300     CALL    WMEDKONV           USING MED-WMEDAREA                        
066400     MOVE    MED-MFSINF         TO    MOD-TEMFSINF                        
066500     .                                                                    
066600     EJECT                                                                
066700 D-STARTA-UTSKRIFT   SECTION.                                             
066800                                                                          
066900     MOVE MID-IDPRT-IN          TO PRT-IDPRTLST                           
067000     MOVE 1                     TO PRT-KDCALL                             
067100     CALL W006PRT USING PRT-W006PRT                                       
067200                                                                          
067300     IF PRT-IDLTERM NOT = 'SAKNAS  '                                      
067400       MOVE MFS-KDMFSFOR        TO PTOP-KDMFSFOR                          
067500       MOVE W-WDQ2CSEQ-IDDISTR  TO MID-IDDISTR                            
067600       MOVE W-WDQ2CSEQ-IDKUNDNR TO MID-IDKUNDNR                           
067700       MOVE W-WDQ2CSEQ-IDKUNDRF TO MID-IDKUNDRF                           
067800       MOVE ZERO                TO MID-IDSID                              
067900       MOVE 'IMS '              TO MID-IDSYSTEM                           
068000       MOVE MID-IDPRT-IN        TO MID-IDPRT                              
068100       MOVE ZERO                TO MID-SUORDV                             
068200       MOVE DAGENS-DATUM        TO MID-TIUPPDAT                           
068300                                   TEST-TIUPPDAT                          
068400                                                                          
068500       MOVE DAGENS-TID          TO MID-TIUPPTID                           
068600                                   TEST-TIUPPTID                          
068700       MOVE LOW-VALUE           TO MID-NYCKEL-GRP                         
068800                                   TEST-NYCKEL                            
068900       PERFORM IMS-INSERT-MSG-ALT                                         
069000       MOVE K-UTSKRIFT-BEGAERD-118 TO MED-IDMFSFEL                        
069100       CALL WMEDKONV USING MED-WMEDAREA                                   
069200       MOVE MED-MFSFEL             TO MOD-TEMFSINF                        
069300     ELSE                                                                 
069400       MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDPRT-IN-ATTR                   
069500       MOVE K-FELAKTIG-PRINTER-772 TO MED-IDMFSFEL                        
069600       CALL WMEDKONV USING MED-WMEDAREA                                   
069700       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 MFS-RENSA-FAELT-UT SECTION.                                              
070200                                                                          
070300     MOVE MFS-RENSA-FAELT TO                                              
070400                             MOD-IDPRT-IN                                 
070500                             MOD-BEKUNDRF                                 
070600                             MOD-IDKAMPRF                                 
070700                             MOD-TIREGDAT                                 
070800                             MOD-TIAAMMDD-RFS                             
070900                             MOD-IDTRPLOS                                 
071000                             MOD-IDTRPVAR                                 
071100                             MOD-TIAAMMDD-TRP                             
071200                             MOD-TIHHMM                                   
071300                             MOD-BEKOPARE                                 
071400                             MOD-ADKOPARE                                 
071500                             MOD-BEGMT-RAD1                               
071600                             MOD-BEGMT-RAD2                               
071700                             MOD-ADGMT-RAD1                               
071800                             MOD-ADGMT-RAD2                               
071900                             MOD-BEBET-RAD1                               
072000                             MOD-BEBET-RAD2                               
072100                             MOD-ADBET-RAD1                               
072200                             MOD-ADBET-RAD2                               
072300                             MOD-BEGDSMRK-DEL1                            
072400                             MOD-BEGDSMRK-DEL2                            
072500     .                                                                    
072600     EJECT                                                                
072700*                                                                         
072800*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
072900*                 III     III MM MMMMM MM SSSS   SSSS                     
073000*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
073100*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
073200*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
073300*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
073400*                 III     III MM MMMMM MM SSSS   SSSS                     
073500*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
073600*                                                                         
073700*                                                                         
073800 IMS-GU-ORQI01 SECTION.                                                   
073900                                                                          
074000     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
074100             DELIMITED BY SIZE INTO SSA1                                  
074200     MOVE    '  GE'              TO GODK-STATUSKODER                      
074300     CALL    CBLTDLI USING       GU   ORQI-PCB    DLI-IO-WLORQI01         
074400                                      SSA1                                
074500     MOVE    ORQI-STATUS-CODE    TO STATUS-WS                             
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800                                                                          
074900 IMS-GNP-ORQI12-FIRST SECTION.                                            
075000                                                                          
075100     STRING  'WLORQI12*F(IDDC     =' W-IDDC-X ')'                         
075200             DELIMITED BY SIZE INTO SSA1                                  
075300     MOVE    '  GE'              TO GODK-STATUSKODER                      
075400     CALL    CBLTDLI USING       GNP  ORQI-PCB    DLI-IO-WLORQI12         
075500                                      SSA1                                
075600     MOVE    ORQI-STATUS-CODE    TO STATUS-WS                             
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     EJECT                                                                
076000 IMS-GU-GMTA01 SECTION.                                                   
076100                                                                          
076200     STRING  'WLGMTA01(IDGMT    =' W-WDB201KEY-X ')'                      
076300            DELIMITED BY SIZE INTO SSA1                                   
076400     MOVE    '    '             TO GODK-STATUSKODER                       
076500     CALL    CBLTDLI USING      GU   GMTA-PCB     DLI-IO-WLGMTA01         
076600                                     SSA1                                 
076700     MOVE    GMTA-STATUS-CODE   TO STATUS-WS                              
076800     PERFORM IMS-STATUSKONTROLL                                           
076900     .                                                                    
077000     EJECT                                                                
077100 IMS-GU-WDB101 SECTION.                                                   
077200                                                                          
077300     STRING  'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                       
077400            DELIMITED BY SIZE INTO SSA1                                   
077500     MOVE    '  GE'             TO GODK-STATUSKODER                       
077600     CALL    CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                 
077700     MOVE    WDB1-STATUS-CODE   TO STATUS-WS                              
077800     PERFORM IMS-STATUSKONTROLL                                           
077900     .                                                                    
078000     EJECT                                                                
078100                                                                          
078200 IMS-GET-MSG SECTION.                                                     
078300                                                                          
078400     MOVE    '  QC'          TO GODK-STATUSKODER                          
078500     CALL    CBLTDLI USING GU MSG-PCB MSG-IO-AREA                         
078600     MOVE    MSG-STATUS-CODE TO STATUS-WS                                 
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900                                                                          
079000 IMS-INSERT-MSG SECTION.                                                  
079100                                                                          
079200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
079300       MOVE '0' TO MFS-KDHUVOMR                                           
079400     END-IF                                                               
079500     MOVE    LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                         
079600     MOVE    SPACE           TO GODK-STATUSKODER                          
079700     CALL    CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD             
079800     MOVE    MSG-STATUS-CODE TO STATUS-WS                                 
079900     PERFORM IMS-STATUSKONTROLL                                           
080000     .                                                                    
080100     EJECT                                                                
080200 IMS-INSERT-MSG-ALT SECTION.                                              
080300                                                                          
080400     MOVE    SPACE           TO GODK-STATUSKODER                          
080500     CALL    CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                         
080600     MOVE    ALT-STATUS-CODE TO STATUS-WS                                 
080700     PERFORM IMS-STATUSKONTROLL                                           
080800     .                                                                    
080900                                                                          
081000 IMS-GU-WDB601    SECTION.                                                
081100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
081200          DELIMITED BY SIZE INTO SSA1                                     
081300     MOVE '  GE' TO GODK-STATUSKODER                                      
081400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
081500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     EJECT                                                                
081900 IMS-STATUSKONTROLL SECTION.                                              
082000                                                                          
082100     SET    STATUS-IX TO +1                                               
082200     SEARCH GODK-STATUS                                                   
082300       AT END                                                             
082400         CALL FELLOG                                                      
082500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
082600         CONTINUE                                                         
082700     END-SEARCH                                                           
082800     .                                                                    
