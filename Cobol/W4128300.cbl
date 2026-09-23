000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4128300.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   09/08/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        VALIDATION OF ORDER RECORDS UPLOADED FROM WEB                    
000900*                                                                         
001000*        THE PROGRAM READS SEVERAL DATABASES VIA SUBROUTINES              
001100*                                                                         
001200*    ABEND CODES                                                          
001300*        U0016 -  IF INPUT FILE IS EMPTY                                  
001400                                                                          
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- DATE AND TIME INFO FROM WEB                                
002100     SELECT INPARM                     ASSIGN TO W41283D0.                
002200                                                                          
002300*          --- RAW ORDER DATA                                             
002400     SELECT W41282                     ASSIGN TO W41283D1.                
002500                                                                          
002600*          --- VALIDATED ORDER DATA                                       
002700     SELECT W41283                     ASSIGN TO W41283D2.                
002800                                                                          
002900*          --- ERROR MAIL                                                 
003000     SELECT W4128E                     ASSIGN TO W41283D4.                
003100*          --- FILE TO REFILL SYSTEM                                      
003200     SELECT W41285                     ASSIGN TO W41283D5.                
003300                                                                          
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003700                                                                          
003800 FD  INPARM                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  FILLER          PIC X(80).                                           
004300     SKIP3                                                                
004400 FD  W41282                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY W4128201      -L.                                              
004900*01  -COPY W4128202      -L.                                              
005000     SKIP3                                                                
005100 FD  W41283                                                               
005200     RECORDING       V                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  RECORD -COPY W41283 -PRE  UT-  -L.                                   
005600     SKIP3                                                                
005700 FD  W4128E                                                               
005800     RECORDING       V                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100 01  EMAIL-RECORD    PIC X(80).                                           
006200                                                                          
006300     EJECT                                                                
006400 FD  W41285                                                               
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700*01  POST -COPY W27111 -PRE  UT2-  -L.                                    
006800                                                                          
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007100                                                                          
007200 77  IDPGM                       PIC X(8)    VALUE 'W4128300'.            
007300 77  FILLER                      PIC X(10)   VALUE 'ERRORTEXT '.          
007400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
007500                                                                          
007600 77  YES                         PIC X       VALUE 'J'.                   
007700 77  NOO                         PIC X       VALUE 'N'.                   
007800 77  OBEHORIG                    PIC X       VALUE 'F'.                   
007900                                                                          
007910 77  UPPER-ALPHA                 PIC X(29)                                
007920                            VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.        
007930 77  LOWER-ALPHA                 PIC X(29)                                
007940                            VALUE 'abcdefghijklmnopqrstuvwxyzåäö'.        
008000*    -- DIFFERENT TYPE OF INPUT FORMAT IN EXCEL FILE                      
008100 77  SPX-IDSYSTEM                PIC X(4)    VALUE 'SPX '.                
008200 77  DEALER-IDSYSTEM             PIC X(4)    VALUE 'XCEL'.                
008300 77  MULTI-DEALER-IDSYSTEM       PIC X(4)    VALUE 'MXCL'.                
008400 77  PROFORMA-IDSYSTEM           PIC X(4)    VALUE 'PXCL'.                
008500 77  REFILL-IDSYSTEM             PIC X(4)    VALUE 'RXCL'.                
008510 77  PREPLANNED-IDSYSTEM         PIC X(4)    VALUE 'PPXC'.                
008600                                                                          
008700*    -- HARD-CODED TPO TYPE IF TPO-DATE IS SPECIFIED                      
008800 77  SPORADIC-KDTPOTYP           PIC 9       VALUE 2.                     
008900                                                                          
009000 77  W41282-EOF-SW               PIC X       VALUE 'N'.                   
009100     88  END-OF-W41282                       VALUE 'J'.                   
009200                                                                          
009300 77  EMAIL-HEADER-WRITTEN-SW     PIC X       VALUE 'N'.                   
009400     88  EMAIL-HEADER-WRITTEN                VALUE 'J'.                   
009500                                                                          
009600 01  CURRENT-DATE-AND-TIME.                                               
009700     03  CURRENT-CENTURY         PIC X(2).                                
009800     03  CURRENT-YEAR            PIC X(2).                                
009900     03  CURRENT-MONTH           PIC X(2).                                
010000     03  CURRENT-DAY             PIC X(2).                                
010100     03  CURRENT-TIMESTAMP.                                               
010200       05 CURRENT-HOUR           PIC X(2).                                
010300       05 CURRENT-MINUTE         PIC X(2).                                
010400       05 CURRENT-SECOND         PIC X(2).                                
010500*      05 CURRENT-HUNDREDTH      PIC X(2).                                
010600                                                                          
010700 01  CURR-DATE.                                                           
010800     03  CURR-YEAR               PIC X(2).                                
010900     03  CURR-MONTH              PIC X(2).                                
011000     03  CURR-DAY                PIC X(2).                                
011100                                                                          
011200 01  GENERAL-SUBPROGRAMS.                                                 
011300*                                                                         
011400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
011700     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
011800     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
011900     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
012000                                                                          
012100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
012200                                                                          
012300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012600                                                                          
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL POSTSUM                                          
012900*                                                                         
013000*01  -COPY W0005   -PRE  POSTSUM-                                         
013100                                                                          
013200*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
013300*   -COPY WSECAREA                                                        
013400     EJECT                                                                
013500 01  IN-AREA-START                PIC X(24)   VALUE                       
013600                                 'IN-AREA-START  '.                       
013700 01  IN-AREA.                                                             
013800     03  IN-AREA-0.                                                       
013900       05  FILLER                PIC X(700).                              
014000*    03  FILLER -COPY W4128201  -PRE IN-  -RED  IN-AREA-0                 
014100*    03  FILLER -COPY W4128202  -PRE IN-  -RED  IN-AREA-0                 
014200     EJECT                                                                
014300 01  INPARM-AREA-START            PIC X(24)   VALUE                       
014400                                 'INPARM-AREA-START  '.                   
014500 01  INPARM-AREA.                                                         
014600     03  INPARM-DATE-AND-TIME    PIC X(14).                               
014700     03  INPARM-IDUSER           PIC X(8).                                
014800     03  FILLER                  PIC X(58).                               
014900                                                                          
015000 01  WORK-AREA-START             PIC X(24)   VALUE                        
015100                                 'WORK-AREA-START         '.              
015200                                                                          
015300 01  W-IDSYSTEM                  PIC X(4).                                
015400 01  W-BEMARKN                   PIC X(24).                               
015500                                                                          
015600 01  W-IDDISTR                   PIC 9(4).                                
015700 01  W-IDKUNDNR                  PIC 9(6).                                
015800 01  W-KDORDKL                   PIC 9.                                   
015900 01  W-REKSIFFR                  PIC 9      VALUE ZERO.                   
016000 01  W-KDFRAKT                   PIC 9(2).                                
016100*01  W-IDDC                      PIC X(2).                                
016200 01  W-FLFORBI                   PIC X(1).                                
016300 01  W-BEKUNDRF                  PIC X(15).                               
016400 01  W-TITPO                     PIC 9(6).                                
016410 01  W-TIREPDAT                  PIC 9(6).                                
016500 01  W-BEGMT-RAD1                PIC X(35).                               
016600 01  W-BEGMT-RAD2                PIC X(35).                               
016700 01  W-ADGMT-GATA                PIC X(35).                               
016800 01  W-ADGMT-PADR                PIC X(35).                               
016900 01  W-ADGMT-LAND                PIC X(35).                               
017000                                                                          
017100 01  W-IDARTNR                   PIC 9(8).                                
017200 01  W-KVBEART                   PIC 9(6).                                
017300 01  W-BERADREF                  PIC X(10).                               
017400 01  W-BEVARREF                  PIC X(10).                               
017500 01  W-IDUSER                    PIC X(8).                                
017600                                                                          
017700 01  W-KDPROTYP                  PIC X(1).                                
017800 01  W-KDFAKTYP                  PIC X(1).                                
017900 01  W-IDKONTO                   PIC 9(10).                               
018000 01  W-IDSKYLT                   PIC X(3).                                
018100 01  W-FORFDAT                   PIC X(6).                                
018200                                                                          
018300 01  W-BEBETRAD-1                PIC X(35).                               
018400 01  W-BEBETRAD-2                PIC X(35).                               
018500 01  W-ADBETRAD-1                PIC X(35).                               
018600 01  W-ADBETRAD-2                PIC X(35).                               
018700 01  W-ADBETRAD-3                PIC X(35).                               
018800                                                                          
018900 01  W-IDDISTR-LENG              PIC S9(3) COMP-3.                        
019000 01  W-IDKUNDNR-LENG             PIC S9(3) COMP-3.                        
019100 01  W-KDORDKL-LENG              PIC S9(3) COMP-3.                        
019200 01  W-KDFRAKT-LENG              PIC S9(3) COMP-3.                        
019300 01  W-IDDC-LENG                 PIC S9(3) COMP-3.                        
019400 01  W-IDDC-REF-LENG             PIC S9(3) COMP-3.                        
019500 01  W-TITPO-LENG                PIC S9(3) COMP-3.                        
019510 01  W-TIREPDAT-LENG             PIC S9(3) COMP-3.                        
019600 01  W-IDARTNR-LENG              PIC S9(3) COMP-3.                        
019700 01  W-KVBEART-LENG              PIC S9(3) COMP-3.                        
019800 01  W-KDPROTYP-LENG             PIC S9(3) COMP-3.                        
019900 01  W-KDFAKTYP-LENG             PIC S9(3) COMP-3.                        
020000 01  W-IDKONTO-LENG              PIC S9(3) COMP-3.                        
020100 01  W-IDSKYLT-LENG              PIC S9(3) COMP-3.                        
020200 01  W-FORFDAT-LENG              PIC S9(3) COMP-3.                        
020300     EJECT                                                                
020400                                                                          
020500*    -- INDEX FOR ERROR-LINES TABLE                                       
020600 01  IX                          PIC S9(4)   BINARY.                      
020700*    -- NUMBER OF ERRORS SO FAR:                                          
020800 01  EX                          PIC S9(4)   BINARY.                      
020900     88  VALID-DATA                          VALUE ZERO.                  
021000                                                                          
021100 01  ERROR-LINES-START           PIC X(24)   VALUE                        
021200                                 'ERROR-LINES-START       '.              
021300 01  ERROR-LINES.                                                         
021400     03  ERROR-LINE OCCURS 20    PIC X(50).                               
021500     EJECT                                                                
021600*    --- PARAMETRAR TILL W411KREG                                         
021700*                                                                         
021800*    -COPY W411KREG                                                       
021900     EJECT                                                                
022000*    --- PARAMETRAR TILL W411AREG                                         
022100*                                                                         
022200*    -COPY W411AREG                                                       
022300     EJECT                                                                
022400*    01  -COPY WWDC99                                                     
022500     EJECT                                                                
022600 01  UT-AREA-START               PIC X(24)   VALUE                        
022700                                 'UT-AREA-START           '.              
022800 01  UT-AREA.                                                             
022900*    03  FILLER -COPY W41283  -PRE UT-                                    
023000     EJECT                                                                
023100 01  UT2-AREA.                                                            
023200*    03  FILLER -COPY W27111  -PRE UT2-                                   
023300     EJECT                                                                
023400 01  EMAIL-AREA-START            PIC X(24)   VALUE                        
023500                                 'EMAIL-AREA-START        '.              
023600                                                                          
023700 01  EMAIL-HDR-1.                                                         
023800     03  FILLER                  PIC X(80)                                
023900     VALUE 'Volvo Car Customer Service'.                                  
024000                                                                          
024100 01  EMAIL-HDR-2.                                                         
024200     03  FILLER                  PIC X(80)                                
024300     VALUE 'The following errors have been detected in your order         
024400-          'transmission.'.                                               
024500                                                                          
024600 01  EMAIL-HDR-2B.                                                        
024700     03  FILLER                  PIC X(80)                                
024800     VALUE 'No order was created.'.                                       
024900                                                                          
025000 01  EMAIL-HDR-3A.                                                        
025100     03  FILLER                  PIC X(8)    VALUE 'Market: '.            
025200     03  EMAIL-BEMARKN           PIC X(24).                               
025300                                                                          
025400 01  EMAIL-HDR-3B.                                                        
025500     03  FILLER                  PIC X(11)   VALUE 'Order ref: '.         
025600     03  EMAIL-BEKUNDRF          PIC X(15).                               
025700                                                                          
025800 01  EMAIL-HDR-4.                                                         
025900     03  FILLER                  PIC X(6)    VALUE 'Date: '.              
026000     03  EMAIL-YEAR              PIC XX.                                  
026100     03  FILLER                  PIC X       VALUE '-'.                   
026200     03  EMAIL-MONTH             PIC XX.                                  
026300     03  FILLER                  PIC X       VALUE '-'.                   
026400     03  EMAIL-DAY               PIC XX.                                  
026500     03  FILLER                  PIC X(7)    VALUE ' Time: '.             
026600     03  EMAIL-HOUR              PIC XX.                                  
026700     03  FILLER                  PIC X       VALUE ':'.                   
026800     03  EMAIL-MINUTE            PIC XX.                                  
026900     03  FILLER                  PIC X       VALUE ':'.                   
027000     03  EMAIL-SECOND            PIC XX.                                  
027100                                                                          
027200 01  EMAIL-HDR-5.                                                         
027300     03  EMAIL-COL1              PIC X(10)   VALUE '          '.          
027400     03  EMAIL-COL2              PIC X(10)   VALUE '          '.          
027500     03  FILLER                  PIC X(10)   VALUE 'Part no.  '.          
027600     03  FILLER                  PIC X(10)   VALUE 'Quantity  '.          
027700                                                                          
027800 01  EMAIL-DATA.                                                          
027900     03  EMAIL-IDDISTR           PIC X(9).                                
028000     03  FILLER                  PIC X       VALUE SPACE.                 
028100     03  EMAIL-IDKUNDNR          PIC X(9).                                
028200     03  FILLER                  PIC X       VALUE SPACE.                 
028300     03  EMAIL-IDARTNR           PIC X(9).                                
028400     03  FILLER                  PIC X       VALUE SPACE.                 
028500     03  EMAIL-KVBEART           PIC X(9).                                
028600     03  FILLER                  PIC X       VALUE SPACE.                 
028700                                                                          
028800 01  EMAIL-ERROR-LINE.                                                    
028900     03  FILLER                  PIC X       VALUE SPACE.                 
029000     03  EMAIL-ERROR-TEXT        PIC X(50).                               
029100                                                                          
029200 01  EMAIL-SPACE                 PIC X       VALUE SPACE.                 
029300     EJECT                                                                
029400 01  DYNAMISKA-SUBPROGRAM.                                                
029500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
029600     SKIP2                                                                
029700 01  STATUS-WS               PIC XX.                                      
029800     88  SEGMENT-FINNS                    VALUE '  '.                     
029900     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
030000     88  SEGMENT-SLUT                     VALUE 'GB'.                     
030100     SKIP2                                                                
030200 01  GODK-STATUSKODER.                                                    
030300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
030400     SKIP2                                                                
030500 01  SSA1                    PIC X(64).                                   
030600 01  SSA2                    PIC X(64).                                   
030700*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
030800                                                                          
030900 01  NYCKLAR-TILL-DLI.                                                    
031000                                                                          
031100   03  W-IDARTNR-2-X.                                                     
031200     05  W-IDARTNR-2         PIC S9(9) COMP-3.                            
031300                                                                          
031400   03  W-IDDC-X.                                                          
031500     05  W-IDDC              PIC X(2).                                    
031600                                                                          
031700   03  W-IDDC-REF-X.                                                      
031800     05  W-IDDC-REF          PIC X(2).                                    
031900                                                                          
032000   03  W-KDSEGKEY-X.                                                      
032100     05  W-KDSEGKEY          PIC X       VALUE '1'.                       
032200                                                                          
032300                                                                          
032400                                                                          
032500*01  -COPY W0003                                                          
032600     EJECT                                                                
032700 01  FILLER                  PIC X(16) VALUE 'IO-AREA-K711'.              
032800 01  IO-AREA-K711.                                                        
032900*    03  -COPY WDK711                                                     
033000     EJECT                                                                
033100                                                                          
033200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC11'.             
033300 01  DLI-IO-ARTC11.                                                       
033400*    03  -COPY WDK611                                                     
033500     EJECT                                                                
033600                                                                          
033700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033800 01   DLI-IO-AREA-B601.                                                   
033900*     03  -COPY WDB601                                                    
034000     EJECT                                                                
034100 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
034200 01   DLI-IO-AREA-B616.                                                   
034300*     03  -COPY WDB616                                                    
034400     EJECT                                                                
034500                                                                          
034600 LINKAGE SECTION.                                                         
034700                                                                          
034800*01  -COPY W0008  -PRE XXKR-                                              
034900     05  FILLER                  PIC X.                                   
035000                                                                          
035100*01  -COPY W0008  -PRE GMTA-                                              
035200     05  FILLER                  PIC X.                                   
035300                                                                          
035400*01  -COPY W0008  -PRE GMTB-                                              
035500     05  FILLER                  PIC X.                                   
035600                                                                          
035700*01  -COPY W0008  -PRE GMTC-                                              
035800     05  FILLER                  PIC X.                                   
035900                                                                          
036000*01  -COPY W0008  -PRE BETC-                                              
036100     05  FILLER                  PIC X.                                   
036200                                                                          
036300*01  -COPY W0008  -PRE XXKB-                                              
036400     05  FILLER                  PIC X.                                   
036500                                                                          
036600*01  -COPY W0008  -PRE ARTC-                                              
036700     05  FILLER                  PIC X.                                   
036800                                                                          
036900*01  -COPY W0008  -PRE ARTS-                                              
037000     05  FILLER                  PIC X.                                   
037100                                                                          
037200*01  -COPY W0008  -PRE WDB6-                                              
037300     05  FILLER                  PIC X.                                   
037400                                                                          
037500     EJECT                                                                
037600 PROCEDURE DIVISION  USING GMTA-PCB  GMTB-PCB                             
037700                           GMTC-PCB  BETC-PCB                             
037800                           ARTC-PCB  ARTS-PCB                             
037900                           WDB6-PCB.                                      
038000 MAIN SECTION.                                                            
038100     ENTRY 'DLITCBL' USING GMTA-PCB  GMTB-PCB                             
038200                           GMTC-PCB  BETC-PCB                             
038300                           ARTC-PCB  ARTS-PCB                             
038400                           WDB6-PCB.                                      
038500                                                                          
038600                                                                          
038700     PERFORM A-INIT                                                       
038710     INITIALIZE  UT-OLIN-W41283                                           
038800     PERFORM B-READ-AND-SAVE-HDR-RCD-DATA                                 
038900                                                                          
039000*    -- PROCESS THE ORDER DATA RECORDS                                    
039100     IF NOT END-OF-W41282                                                 
039200       PERFORM S01-READ-W41282                                            
039300     END-IF                                                               
039400     PERFORM UNTIL END-OF-W41282                                          
039500                                                                          
039600       IF W-IDSYSTEM = REFILL-IDSYSTEM                                    
039700         PERFORM E-VALIDATE-REFILL                                        
039800       ELSE                                                               
039900         PERFORM C-VALIDATE-DATA                                          
040000       END-IF                                                             
040100       IF VALID-DATA                                                      
040200         IF (W-IDSYSTEM = REFILL-IDSYSTEM)                                
040300         AND W-FLFORBI = 'N'                                              
040400           PERFORM S12-WRITE-W41285                                       
040500         ELSE                                                             
040600           IF W-IDSYSTEM NOT = REFILL-IDSYSTEM                            
040700             PERFORM D-WRITE-ORDER-DATA                                   
040800           ELSE                                                           
040900             PERFORM S11-WRITE-W41283                                     
041000           END-IF                                                         
041100         END-IF                                                           
041200       ELSE                                                               
041300         PERFORM F-WRITE-ERROR-MAIL-DATA                                  
041400       END-IF                                                             
041500                                                                          
041600       PERFORM S01-READ-W41282                                            
041700     END-PERFORM                                                          
041800                                                                          
041900     PERFORM Z-FINIT                                                      
042000     MOVE ZERO TO RETURN-CODE                                             
042100     GOBACK                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 A-INIT SECTION.                                                          
042500                                                                          
042600     OPEN INPUT  INPARM W41282                                            
042700     OPEN OUTPUT W41283 W4128E W41285                                     
042800                                                                          
042900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
043000                                                                          
043100     READ INPARM INTO INPARM-AREA                                         
043200     AT END                                                               
043300*      -- WHY NOT?                                                        
043400       MOVE FUNCTION CURRENT-DATE(1:14) TO CURRENT-DATE-AND-TIME          
043500**     MOVE SPACE                       TO W-IDUSER                       
043600     END-READ                                                             
043700                                                                          
043800     MOVE INPARM-DATE-AND-TIME                                            
043900                            TO CURRENT-DATE-AND-TIME                      
044000     MOVE INPARM-IDUSER     TO W-IDUSER                                   
044100     MOVE CURRENT-YEAR      TO EMAIL-YEAR                                 
044200     MOVE CURRENT-MONTH     TO EMAIL-MONTH                                
044300     MOVE CURRENT-DAY       TO EMAIL-DAY                                  
044400     MOVE CURRENT-HOUR      TO EMAIL-HOUR                                 
044500     MOVE CURRENT-MINUTE    TO EMAIL-MINUTE                               
044600     MOVE CURRENT-SECOND    TO EMAIL-SECOND                               
044700     .                                                                    
044800                                                                          
044900     EJECT                                                                
045000 B-READ-AND-SAVE-HDR-RCD-DATA SECTION.                                    
045100                                                                          
045200     PERFORM S01-READ-W41282                                              
045300     IF NOT END-OF-W41282                                                 
045400       MOVE IN-OHDR-IDSYSTEM TO W-IDSYSTEM                                
045500       MOVE IN-OHDR-BEMARKN  TO W-BEMARKN                                 
045600     ELSE                                                                 
045700       ADD 1 TO EX                                                        
045800       MOVE 'No valid data in order file.' TO ERROR-LINE(EX)              
045900       ADD 1 TO EX                                                        
046000       MOVE 'Can not find any order lines.' TO ERROR-LINE(EX)             
046100*      -- CLEAR INPUT FIELDS TO MAKE A NICER ERROR MAIL                   
046200       MOVE SPACE TO IN-AREA, W-BEKUNDRF                                  
046300*      -- SORRY FOR THIS UNSTRUCTERED PERFORM                             
046400       PERFORM F-WRITE-ERROR-MAIL-DATA                                    
046500     END-IF                                                               
046600     .                                                                    
046700                                                                          
046800     EJECT                                                                
046900 C-VALIDATE-DATA SECTION.                                                 
047000                                                                          
047100*    -- NO ERRORS YET FOR THIS ORDER DATA RECORD                          
047200*    -- (THIS WILL SET ERROR-TEXT INDEX TO ZERO)                          
047300     SET VALID-DATA TO TRUE                                               
047400                                                                          
047500*    -- DISTRICT --                                                       
047600     UNSTRING IN-ODTA-IDDISTR    DELIMITED BY SPACE                       
047700              INTO W-IDDISTR                                              
047800              COUNT IN W-IDDISTR-LENG                                     
047900     IF W-IDDISTR-LENG > LENGTH OF W-IDDISTR                              
048000        ADD 1 TO EX                                                       
048100        MOVE 'District no. is too long'    TO ERROR-LINE(EX)              
048200     END-IF                                                               
048300     IF W-IDDISTR NOT NUMERIC                                             
048400        ADD 1 TO EX                                                       
048500        MOVE 'District no. is not numeric' TO ERROR-LINE(EX)              
048600     END-IF                                                               
048700                                                                          
048800*    -- DEALER --                                                         
048900     UNSTRING IN-ODTA-IDKUNDNR   DELIMITED BY SPACE                       
049000              INTO W-IDKUNDNR                                             
049100              COUNT IN W-IDKUNDNR-LENG                                    
049200     IF W-IDKUNDNR-LENG > LENGTH OF W-IDKUNDNR                            
049300        ADD 1 TO EX                                                       
049400        MOVE 'Dealer no. is too long'    TO ERROR-LINE(EX)                
049500     END-IF                                                               
049600     IF W-IDKUNDNR NOT NUMERIC                                            
049700        ADD 1 TO EX                                                       
049800        MOVE 'Dealer no. is not numeric' TO ERROR-LINE(EX)                
049900     END-IF                                                               
050000                                                                          
050100     IF W-IDDISTR IS NUMERIC AND W-IDKUNDNR IS NUMERIC                    
050200        PERFORM CA-VALIDATE-DISTR-AND-CUST                                
050300     END-IF                                                               
050400                                                                          
050500*    -- IDUSER --                                                         
050600     IF W-IDSYSTEM NOT = SPX-IDSYSTEM                                     
050700        PERFORM CC-VALIDATE-IDUSER                                        
050800     END-IF                                                               
050900                                                                          
051000*    -- ORDER CLASS --                                                    
051100     UNSTRING IN-ODTA-KDORDKL    DELIMITED BY SPACE                       
051200              INTO W-KDORDKL                                              
051300              COUNT IN W-KDORDKL-LENG                                     
051400     IF W-KDORDKL-LENG > LENGTH OF W-KDORDKL                              
051500        ADD 1 TO EX                                                       
051600        MOVE 'Order class is too long'    TO ERROR-LINE(EX)               
051700     END-IF                                                               
051800     IF W-KDORDKL NOT NUMERIC                                             
051900        ADD 1 TO EX                                                       
052000        MOVE 'Order class must be numeric' TO ERROR-LINE(EX)              
052100     ELSE                                                                 
052200        IF W-KDORDKL > 4                                                  
052300          ADD 1 TO EX                                                     
052400          MOVE 'Invalid Order class' TO ERROR-LINE(EX)                    
052500        END-IF                                                            
052600     END-IF                                                               
052700                                                                          
052800*    -- FREIGHT CODE --                                                   
052900     UNSTRING IN-ODTA-KDFRAKT    DELIMITED BY SPACE                       
053000              INTO W-KDFRAKT                                              
053100              COUNT IN W-KDFRAKT-LENG                                     
053200     IF W-KDFRAKT-LENG > LENGTH OF W-KDFRAKT                              
053300        ADD 1 TO EX                                                       
053400        MOVE 'Freight code is too long' TO ERROR-LINE(EX)                 
053500     END-IF                                                               
053600     IF W-KDFRAKT NOT NUMERIC                                             
053700        ADD 1 TO EX                                                       
053800        MOVE 'Freight code must be numeric' TO ERROR-LINE(EX)             
053900     END-IF                                                               
054000                                                                          
054100*    -- DISTRIBUTION CENTER --                                            
054200     UNSTRING IN-ODTA-IDDC    DELIMITED BY SPACE                          
054300              INTO W-IDDC                                                 
054400              COUNT IN W-IDDC-LENG                                        
054500     IF W-IDDC-LENG > LENGTH OF W-IDDC                                    
054600        ADD 1 TO EX                                                       
054700        MOVE 'Distribution center is invalid' TO ERROR-LINE(EX)           
054800     END-IF                                                               
054900     IF IN-ODTA-FLFORBI = YES AND                                         
055000        W-IDDC  = SPACE                                                   
055100        ADD 1 TO EX                                                       
055200        MOVE 'DC must be given' TO ERROR-LINE(EX)                         
055300     END-IF                                                               
055400                                                                          
055500*    -- TPO DATE --                                                       
055600     UNSTRING IN-ODTA-TITPO    DELIMITED BY SPACE                         
055700              INTO W-TITPO                                                
055800              COUNT IN W-TITPO-LENG                                       
055900     IF W-TITPO-LENG > LENGTH OF W-TITPO                                  
056000        ADD 1 TO EX                                                       
056100        MOVE 'TPO date is too long' TO ERROR-LINE(EX)                     
056200     END-IF                                                               
056300     IF W-TITPO NOT NUMERIC                                               
056400        ADD 1 TO EX                                                       
056500        MOVE 'TPO date is invalid' TO ERROR-LINE(EX)                      
056600     END-IF                                                               
056700                                                                          
056710*    -- REPAIR DATE --                                                    
056720     UNSTRING IN-ODTA-TIREPDAT DELIMITED BY SPACE                         
056730              INTO W-TIREPDAT                                             
056740              COUNT IN W-TIREPDAT-LENG                                    
056750     IF W-TIREPDAT-LENG > LENGTH OF W-TIREPDAT                            
056760        ADD 1 TO EX                                                       
056770        MOVE 'Rep.date is too long' TO ERROR-LINE(EX)                     
056780     END-IF                                                               
056790     IF W-TIREPDAT NOT NUMERIC                                            
056791        ADD 1 TO EX                                                       
056792        MOVE 'Rep.date is invalid' TO ERROR-LINE(EX)                      
056793     END-IF                                                               
056794                                                                          
056800*    -- PART NUMBER --                                                    
056900     UNSTRING IN-ODTA-IDARTNR    DELIMITED BY SPACE                       
057000              INTO W-IDARTNR                                              
057100              COUNT IN W-IDARTNR-LENG                                     
057200     IF W-IDARTNR-LENG > LENGTH OF W-IDARTNR                              
057300        ADD 1 TO EX                                                       
057400        MOVE 'Part no. is too long'    TO ERROR-LINE(EX)                  
057500     END-IF                                                               
057600     IF W-IDARTNR NOT NUMERIC                                             
057700        ADD 1 TO EX                                                       
057800        MOVE 'Part no. is not numeric' TO ERROR-LINE(EX)                  
057900     END-IF                                                               
058000                                                                          
058100     IF W-IDARTNR IS NUMERIC                                              
058200        PERFORM CB-VALIDATE-PART                                          
058300     END-IF                                                               
058400                                                                          
058500*    -- ORDERED QUANTITY --                                               
058600     UNSTRING IN-ODTA-KVBEART    DELIMITED BY SPACE                       
058700              INTO W-KVBEART                                              
058800              COUNT IN W-KVBEART-LENG                                     
058900     IF W-KVBEART-LENG > LENGTH OF W-KVBEART                              
059000        ADD 1 TO EX                                                       
059100        MOVE 'Ordered quantity is too long'    TO ERROR-LINE(EX)          
059200     END-IF                                                               
059300     IF W-KVBEART NOT NUMERIC                                             
059400        ADD 1 TO EX                                                       
059500        MOVE 'Ordered quantity is not numeric' TO ERROR-LINE(EX)          
059600     END-IF                                                               
059700                                                                          
059800*    -- PROFORMA TYPE    --                                               
059900     IF W-IDSYSTEM = PROFORMA-IDSYSTEM                                    
060000        UNSTRING IN-ODTA-KDPROTYP DELIMITED BY SPACE                      
060100                 INTO W-KDPROTYP                                          
060200                 COUNT IN W-KDPROTYP-LENG                                 
060300        IF W-KDPROTYP-LENG > LENGTH OF W-KDPROTYP                         
060400           ADD 1 TO EX                                                    
060500           MOVE 'Proforma type is too long' TO ERROR-LINE(EX)             
060600        END-IF                                                            
060700        IF W-KDPROTYP = 'F' OR 'O' OR 'L'                                 
060800           CONTINUE                                                       
060900        ELSE                                                              
061000           ADD 1 TO EX                                                    
061100           MOVE 'Proforma type is not F, O or L' TO ERROR-LINE(EX)        
061200        END-IF                                                            
061300     END-IF                                                               
061400                                                                          
061500*    -- INVOICE TYPE     --                                               
061600     IF W-IDSYSTEM = PROFORMA-IDSYSTEM                                    
061700        UNSTRING IN-ODTA-KDFAKTYP DELIMITED BY SPACE                      
061800                 INTO W-KDFAKTYP                                          
061900                 COUNT IN W-KDFAKTYP-LENG                                 
062000        IF W-KDFAKTYP-LENG > LENGTH OF W-KDFAKTYP                         
062100           ADD 1 TO EX                                                    
062200           MOVE 'Invoice type is too long' TO ERROR-LINE(EX)              
062300        END-IF                                                            
062400        IF W-KDFAKTYP = 'R' OR 'K' OR 'N' OR 'G' OR 'F' OR 'P'            
062500           CONTINUE                                                       
062600        ELSE                                                              
062700           ADD 1 TO EX                                                    
062800           MOVE 'Invoice type is not R, K, N, G, or F'                    
062900                                              TO ERROR-LINE(EX)           
063000        END-IF                                                            
063100     END-IF                                                               
063200                                                                          
063300*    -- ACCOUNT          --                                               
063400     IF W-IDSYSTEM = PROFORMA-IDSYSTEM                                    
063500        UNSTRING IN-ODTA-IDKONTO DELIMITED BY SPACE                       
063600                 INTO W-IDKONTO                                           
063700                 COUNT IN W-IDKONTO-LENG                                  
063800        IF W-IDKONTO-LENG > LENGTH OF W-IDKONTO                           
063900           ADD 1 TO EX                                                    
064000           MOVE 'Account is too long' TO ERROR-LINE(EX)                   
064100        END-IF                                                            
064200        IF W-IDKONTO not numeric                                          
064300           ADD 1 TO EX                                                    
064400           MOVE 'Account is not numeric' TO ERROR-LINE(EX)                
064500        END-IF                                                            
064600     END-IF                                                               
064700                                                                          
064800*    -- LANGUAGE         --                                               
064900     IF W-IDSYSTEM = PROFORMA-IDSYSTEM                                    
065000        UNSTRING IN-ODTA-IDSKYLT DELIMITED BY SPACE                       
065100                 INTO W-IDSKYLT                                           
065200                 COUNT IN W-IDSKYLT-LENG                                  
065300        IF W-IDSKYLT-LENG > LENGTH OF W-IDSKYLT                           
065400           ADD 1 TO EX                                                    
065500           MOVE 'Language is too long' TO ERROR-LINE(EX)                  
065600        ELSE                                                              
065700           MOVE W-IDSKYLT TO UT-OLIN-IDSKYLT                              
065800           IF NOT UT-OLIN-GODK-IDSKYLT                                    
065900              ADD 1 TO EX                                                 
066000              MOVE 'Undefined language' TO ERROR-LINE(EX)                 
066100           END-IF                                                         
066200        END-IF                                                            
066300     END-IF                                                               
066400                                                                          
066500*    -- DUE DATE         --                                               
066600     IF W-IDSYSTEM = PROFORMA-IDSYSTEM                                    
066700        UNSTRING IN-ODTA-FORFDAT DELIMITED BY SPACE                       
066800                 INTO W-FORFDAT                                           
066900                 COUNT IN W-FORFDAT-LENG                                  
067000        IF W-FORFDAT-LENG > LENGTH OF W-FORFDAT                           
067100           ADD 1 TO EX                                                    
067200           MOVE 'Dua date is too long' TO ERROR-LINE(EX)                  
067300        END-IF                                                            
067400        MOVE CURRENT-YEAR     TO CURR-YEAR                                
067500        MOVE CURRENT-MONTH    TO CURR-MONTH                               
067600        MOVE CURRENT-DAY      TO CURR-DAY                                 
067700        IF W-FORFDAT NOT NUMERIC                                          
067800        OR W-FORFDAT NOT > CURR-DATE                                      
067900           ADD 1 TO EX                                                    
068000           MOVE 'Due date is invalid' TO ERROR-LINE(EX)                   
068100        END-IF                                                            
068200     END-IF                                                               
068300                                                                          
068400*    -- NO VALIDATION OF THE FOLLOWING FIELDS                             
068500     MOVE IN-ODTA-BEKUNDRF    TO W-BEKUNDRF                               
068600     MOVE IN-ODTA-BERADREF    TO W-BERADREF                               
068700     MOVE IN-ODTA-BEVARREF    TO W-BEVARREF                               
068800     MOVE IN-ODTA-BEGMT-RAD1  TO W-BEGMT-RAD1                             
068900     MOVE IN-ODTA-BEGMT-RAD2  TO W-BEGMT-RAD2                             
069000     MOVE IN-ODTA-ADGMT-GATA  TO W-ADGMT-GATA                             
069100     MOVE IN-ODTA-ADGMT-PADR  TO W-ADGMT-PADR                             
069200     MOVE IN-ODTA-ADGMT-LAND  TO W-ADGMT-LAND                             
069300     MOVE IN-ODTA-FLFORBI     TO W-FLFORBI                                
069400     MOVE IN-ODTA-BEBETRAD-1  TO W-BEBETRAD-1                             
069500     MOVE IN-ODTA-BEBETRAD-2  TO W-BEBETRAD-2                             
069600     MOVE IN-ODTA-ADBETRAD-1  TO W-ADBETRAD-1                             
069700     MOVE IN-ODTA-ADBETRAD-2  TO W-ADBETRAD-2                             
069800     MOVE IN-ODTA-ADBETRAD-3  TO W-ADBETRAD-3                             
069900     .                                                                    
070000                                                                          
070100     EJECT                                                                
070200 CA-VALIDATE-DISTR-AND-CUST SECTION.                                      
070300                                                                          
070400     INITIALIZE KREG-W411KREG                                             
070500     MOVE W-IDSYSTEM  TO KREG-IDSYSTEM                                    
070600     MOVE W-IDDISTR   TO KREG-IDDISTR                                     
070700     MOVE W-IDKUNDNR  TO KREG-IDKUNDNR                                    
070800     MOVE W-KDORDKL   TO KREG-KDORDKL                                     
070900                                                                          
071000     CALL W411KREG USING KREG-W411KREG                                    
071100                         GMTA-PCB      GMTB-PCB                           
071200                         GMTC-PCB      BETC-PCB.                          
071300                                                                          
071400     IF KREG-IDDISTR-OK = NOO OR KREG-IDKUNDNR-OK = NOO                   
071500        ADD 1 TO EX                                                       
071600        MOVE 'District/Dealer not found in Customers database'            
071700        TO ERROR-LINE(EX)                                                 
071800     END-IF                                                               
071900     .                                                                    
072000                                                                          
072100     EJECT                                                                
072200 CB-VALIDATE-PART   SECTION.                                              
072300                                                                          
072400     INITIALIZE AREG-W411AREG                                             
072500     MOVE W-IDARTNR   TO AREG-IDARTNR                                     
072600     MOVE KREG-IDDC   TO AREG-IDDC                                        
072700                                                                          
072800     CALL W411AREG USING AREG-W411AREG                                    
072900                         ARTC-PCB      ARTS-PCB                           
073000                                                                          
073100     MOVE AREG-REKSIFFR TO W-REKSIFFR                                     
073200     IF AREG-KDORDBEK = 58                                                
073300        ADD 1 TO EX                                                       
073400        MOVE 'Part no. not found in Parts database'                       
073500        TO ERROR-LINE(EX)                                                 
073600     END-IF                                                               
073700     IF AREG-KDERS-UTG > 0                                                
073800        ADD 1 TO EX                                                       
073900        MOVE 'Part is obsoleted'                                          
074000        TO ERROR-LINE(EX)                                                 
074100     END-IF                                                               
074200                                                                          
074300     .                                                                    
074400                                                                          
074500     EJECT                                                                
074600 CC-VALIDATE-IDUSER SECTION.                                              
074700                                                                          
074800     IF W-IDUSER = SPACE                                                  
074900       ADD 1 TO EX                                                        
075000       MOVE 'User missing and not authorized.'                            
075100       TO ERROR-LINE(EX)                                                  
075200     ELSE                                                                 
075300       MOVE W-IDUSER              TO SEC-IDUSER                           
075400       IF W-IDSYSTEM = REFILL-IDSYSTEM                                    
075500***users that have permission to update on 2352 has permission            
075600***to upload files for refill                                             
075700         MOVE 'W2T352U'           TO SEC-IDTRANS                          
075800         MOVE SPACE               TO SEC-IDKEY                            
075900       ELSE                                                               
076000         MOVE SPACE               TO SEC-IDTRANS                          
076100         MOVE W-IDDISTR           TO SEC-IDKEY                            
076200       END-IF                                                             
076300                                                                          
076400       CALL WSECURIT USING SEC-IDUSER                                     
076500                           SEC-IDTRANS                                    
076600                           SEC-IDKEY                                      
076700                           SEC-KDSVAR                                     
076800                                                                          
076900       IF SEC-KDSVAR = OBEHORIG                                           
077000       ADD 1 TO EX                                                        
077100       MOVE 'User not authorized in WSECURIT.'                            
077200       TO ERROR-LINE(EX)                                                  
077300       END-IF                                                             
077400     END-IF                                                               
077500                                                                          
077600     .                                                                    
077700                                                                          
077800     EJECT                                                                
077900 D-WRITE-ORDER-DATA SECTION.                                              
078000                                                                          
078100     IF W-IDSYSTEM = MULTI-DEALER-IDSYSTEM OR                             
078110                     PREPLANNED-IDSYSTEM                                  
078120*    IF W-IDSYSTEM = MULTI-DEALER-IDSYSTEM                                
078200*      -- MULTI-DEALER-IDSYSTEM IS ONLY SIGNIFICANT                       
078300*      -- IN THIS PROGRAM. CHANGE TO NORMAL "EXCEL" IDSYSTEM              
078400       MOVE DEALER-IDSYSTEM TO UT-OLIN-IDSYSTEM                           
078500     ELSE                                                                 
078600       MOVE W-IDSYSTEM      TO UT-OLIN-IDSYSTEM                           
078700     END-IF                                                               
078800                                                                          
078900     MOVE W-BEMARKN     TO UT-OLIN-BEMARKN                                
079000     MOVE W-IDDISTR     TO UT-OLIN-IDDISTR                                
079100     MOVE W-IDKUNDNR    TO UT-OLIN-IDKUNDNR                               
079200     MOVE W-KDORDKL     TO UT-OLIN-KDORDKL                                
079300     MOVE W-FLFORBI     TO UT-OLIN-FLFORBI                                
079400                                                                          
079500     IF W-IDSYSTEM = SPX-IDSYSTEM                                         
079600        MOVE CURRENT-TIMESTAMP TO UT-OLIN-BEKUNDRF                        
079700     ELSE                                                                 
079800*       -- FROM INPUT FILE                                                
079900        MOVE W-BEKUNDRF        TO UT-OLIN-BEKUNDRF                        
080000     END-IF                                                               
080100                                                                          
080200     MOVE W-KDFRAKT     TO UT-OLIN-KDFRAKT                                
080300     MOVE W-IDDC        TO UT-OLIN-IDDC                                   
080400     IF W-TITPO > ZERO                                                    
080500*      -- FIXED VALUE IF TPO-DATE IS SPECIFIED                            
080600       MOVE SPORADIC-KDTPOTYP TO UT-OLIN-KDTPOTYP                         
080700     ELSE                                                                 
080800       MOVE ZERO              TO UT-OLIN-KDTPOTYP                         
080900     END-IF                                                               
081000     MOVE W-TITPO       TO UT-OLIN-TITPO                                  
081010     MOVE W-TIREPDAT    TO UT-OLIN-TIREPDAT                               
081100     MOVE W-BEGMT-RAD1  TO UT-OLIN-BEGMT-RAD1                             
081200     MOVE W-BEGMT-RAD2  TO UT-OLIN-BEGMT-RAD2                             
081300     MOVE W-ADGMT-GATA  TO UT-OLIN-ADGMT-GATA                             
081400     MOVE W-ADGMT-PADR  TO UT-OLIN-ADGMT-PADR                             
081500     MOVE W-ADGMT-LAND  TO UT-OLIN-ADGMT-LAND                             
081600                                                                          
081700     MOVE W-IDARTNR     TO UT-OLIN-IDARTNR                                
081800     MOVE W-KVBEART     TO UT-OLIN-KVBEART                                
081900     MOVE W-BERADREF    TO UT-OLIN-BERADREF                               
082000     MOVE W-BEVARREF    TO UT-OLIN-BEVARREF                               
082100                                                                          
082200*    -- CHECK DIGIT FROM W411AREG                                         
082300     MOVE W-REKSIFFR TO UT-OLIN-REKSIFFR                                  
082400                                                                          
082500     IF UT-OLIN-IDSYSTEM = PROFORMA-IDSYSTEM                              
082600        MOVE W-KDPROTYP   TO UT-OLIN-KDPROTYP                             
082700        MOVE W-KDFAKTYP   TO UT-OLIN-KDFAKTYP                             
082800        MOVE W-IDKONTO    TO UT-OLIN-IDKONTO                              
082900        MOVE W-IDSKYLT    TO UT-OLIN-IDSKYLT                              
083000        MOVE W-FORFDAT    TO UT-OLIN-FORFDAT                              
083100                                                                          
083200        MOVE W-BEBETRAD-1 TO UT-OLIN-BEBETRAD-1                           
083300        MOVE W-BEBETRAD-2 TO UT-OLIN-BEBETRAD-2                           
083400        MOVE W-ADBETRAD-1 TO UT-OLIN-ADBETRAD-1                           
083500        MOVE W-ADBETRAD-2 TO UT-OLIN-ADBETRAD-2                           
083600        MOVE W-ADBETRAD-3 TO UT-OLIN-ADBETRAD-3                           
083700     END-IF                                                               
083800     PERFORM S11-WRITE-W41283                                             
083900     .                                                                    
084000                                                                          
084100     EJECT                                                                
084200 E-VALIDATE-REFILL SECTION.                                               
084300                                                                          
084400     INITIALIZE UT2-AREA                                                  
084500*    -- NO ERRORS YET FOR THIS ORDER DATA RECORD                          
084600*    -- (THIS WILL SET ERROR-TEXT INDEX TO ZERO)                          
084700     SET VALID-DATA TO TRUE                                               
084800                                                                          
084900*    -- IDUSER --                                                         
085000     IF W-IDSYSTEM NOT = SPX-IDSYSTEM                                     
085100        PERFORM CC-VALIDATE-IDUSER                                        
085200     END-IF                                                               
085300                                                                          
085400*    -- ORDER CLASS --                                                    
085500     UNSTRING IN-ODTA-KDORDKL    DELIMITED BY SPACE                       
085600              INTO W-KDORDKL                                              
085700              COUNT IN W-KDORDKL-LENG                                     
085800     IF W-KDORDKL-LENG > LENGTH OF W-KDORDKL                              
085900        ADD 1 TO EX                                                       
086000        MOVE 'Order class is too long'    TO ERROR-LINE(EX)               
086100     END-IF                                                               
086200     IF W-KDORDKL NOT NUMERIC                                             
086300        ADD 1 TO EX                                                       
086400        MOVE 'Order class must be numeric' TO ERROR-LINE(EX)              
086500     ELSE                                                                 
086600        IF W-KDORDKL > 4                                                  
086700          ADD 1 TO EX                                                     
086800          MOVE 'Invalid Order class' TO ERROR-LINE(EX)                    
086900        END-IF                                                            
087000     END-IF                                                               
087100                                                                          
087200*    -- PART NUMBER --                                                    
087300     MOVE ZERO             TO W-IDARTNR-2                                 
087400     UNSTRING IN-ODTA-IDARTNR    DELIMITED BY SPACE                       
087500              INTO W-IDARTNR                                              
087600              COUNT IN W-IDARTNR-LENG                                     
087700     IF W-IDARTNR-LENG > LENGTH OF W-IDARTNR                              
087800        ADD 1 TO EX                                                       
087900        MOVE 'Part no. is too long'    TO ERROR-LINE(EX)                  
088000     END-IF                                                               
088100     IF W-IDARTNR NOT NUMERIC                                             
088200        ADD 1 TO EX                                                       
088300        MOVE 'Part no. is not numeric' TO ERROR-LINE(EX)                  
088400     ELSE                                                                 
088500        MOVE W-IDARTNR     TO W-IDARTNR-2                                 
088600     END-IF                                                               
088700                                                                          
088800     IF W-IDARTNR IS NUMERIC                                              
088900        IF W-IDARTNR > 0                                                  
089000          PERFORM CB-VALIDATE-PART                                        
089100        ELSE                                                              
089200          ADD 1 TO EX                                                     
089300          MOVE 'Part no. is wrong/missing' TO ERROR-LINE(EX)              
089400        END-IF                                                            
089500     END-IF                                                               
089600                                                                          
089610     UNSTRING IN-ODTA-IDDC    DELIMITED BY SPACE                          
089620              INTO W-IDDC                                                 
089630              COUNT IN W-IDDC-LENG                                        
089631     INSPECT W-IDDC                                                       
089632        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
089633*****because this is the sending dc when bypass orders                    
089634     MOVE W-IDDC     TO W-IDDC-REF                                        
089635******************************************************                    
089640     IF W-IDDC-LENG > LENGTH OF W-IDDC                                    
089650        ADD 1 TO EX                                                       
089660        MOVE 'Distribution center is invalid' TO ERROR-LINE(EX)           
089670     END-IF                                                               
089680     IF IN-ODTA-FLFORBI = YES AND                                         
089690        W-IDDC  = SPACE                                                   
089691        ADD 1 TO EX                                                       
089692        MOVE 'DC must be given' TO ERROR-LINE(EX)                         
089693     END-IF                                                               
089700*    -- ORDERED QUANTITY --                                               
089800     UNSTRING IN-ODTA-KVBEART    DELIMITED BY SPACE                       
089900              INTO W-KVBEART                                              
090000              COUNT IN W-KVBEART-LENG                                     
090100     IF W-KVBEART-LENG > LENGTH OF W-KVBEART                              
090200        ADD 1 TO EX                                                       
090300        MOVE 'Ordered quantity is too long'    TO ERROR-LINE(EX)          
090400     END-IF                                                               
090500     IF W-KVBEART NOT NUMERIC                                             
090600        ADD 1 TO EX                                                       
090700        MOVE 'Ordered quantity is not numeric' TO ERROR-LINE(EX)          
090800     ELSE                                                                 
090900        IF W-KVBEART < 1                                                  
091000          ADD 1 TO EX                                                     
091100          MOVE 'Ordered quantity is ZERO' TO ERROR-LINE(EX)               
091200        END-IF                                                            
091300     END-IF                                                               
091400     IF IN-ODTA-FLFORBI = 'N'                                             
091500       PERFORM EA-APPROVED-REFILL-PROPOSAL                                
091600     ELSE                                                                 
091700       PERFORM EB-REFILL-BYPASS-ORDER                                     
091800     END-IF                                                               
091900     MOVE IN-ODTA-FLFORBI     TO W-FLFORBI                                
093400     .                                                                    
093500                                                                          
093600     EJECT                                                                
093700 EA-APPROVED-REFILL-PROPOSAL SECTION.                                     
093800*    -- FREIGHT CODE --                                                   
093900       UNSTRING IN-ODTA-KDFRAKT  DELIMITED BY SPACE                       
094000                INTO W-KDFRAKT                                            
094100                COUNT IN W-KDFRAKT-LENG                                   
094200       IF W-KDFRAKT-LENG > LENGTH OF W-KDFRAKT                            
094300          ADD 1 TO EX                                                     
094400          MOVE 'Freight code is too long' TO ERROR-LINE(EX)               
094500       END-IF                                                             
094600       IF W-KDFRAKT NOT NUMERIC                                           
094700          ADD 1 TO EX                                                     
094800          MOVE 'Freight code must be numeric' TO ERROR-LINE(EX)           
094900       END-IF                                                             
095000                                                                          
095100       PERFORM IMS-GU-WDK711                                              
095200       IF SEGMENT-FINNS                                                   
095300         IF SLAG-IDDC-REF = SPACE                                         
095400          ADD 1 TO EX                                                     
095500          MOVE 'Can not refill to purchase DC'  TO ERROR-LINE(EX)         
095600         ELSE                                                             
095700          MOVE SLAG-IDDC-REF        TO W-IDDC-REF                         
095800          MOVE SLAG-IDDC            TO UT2-IDDC                           
095900          MOVE SLAG-IDLEVNR         TO UT2-IDLEVNR                        
096000          MOVE SLAG-ADLAGOMR        TO UT2-ADLAGOMR-SDC                   
096100          MOVE SLAG-ADGANG          TO UT2-ADGANG-SDC                     
096200          MOVE SLAG-ADPLATS         TO UT2-ADPLATS-SDC                    
096210          MOVE SLAG-IDPERSON-BUY    TO UT2-IDPERSON-BUY                   
096300          IF LDC OR SDC                                                   
096400**WE   NEVER FLY REFILL TO SDC/LDC                                        
096500            MOVE 'B'                TO UT2-KDREFTYP                       
096600          ELSE                                                            
096700            IF IN-ODTA-KDORDKL = 1                                        
096800              MOVE 'A'              TO UT2-KDREFTYP                       
096900            ELSE                                                          
097000              MOVE 'B'              TO UT2-KDREFTYP                       
097100            END-IF                                                        
097200          END-IF                                                          
097300          MOVE W-IDARTNR            TO UT2-IDARTNR                        
097400          MOVE W-KVBEART            TO UT2-KVBEART                        
097500          MOVE 'O'                  TO UT2-KDREFORS                       
097600          MOVE W-KDFRAKT            TO UT2-KDFRAKT                        
097800          MOVE ZERO                 TO   UT2-KDREFTXT                     
097900                                         UT2-KVBEART-CD                   
098000                                         UT2-ADLAGOMR-CD                  
098100                                         UT2-ADGANG-CD                    
098200                                         UT2-ADPLATS-CD                   
098300                                         UT2-IDKUNDNR                     
098400          MOVE SLAG-IDDC-REF        TO UT2-IDDC-REF                       
098500          PERFORM IMS-GU-WDB601                                           
098600          IF SEGMENT-FINNS                                                
098700            IF W-IDDC-REF = '11'                                          
098710              IF DCS-IDDISTR-REFILL = ZERO                                
098720                ADD 1 TO EX                                               
098730                MOVE 'District zero,look 4404 screen'                     
098731                                              TO ERROR-LINE(EX)           
098740              ELSE                                                        
098800                MOVE DCS-IDDISTR-REFILL TO UT2-IDDISTR                    
098900**                                    so you get dist in errormail        
099000                                           IN-ODTA-IDDISTR                
099010              END-IF                                                      
099100              PERFORM IMS-GU-WDK611                                       
099200              IF SEGMENT-FINNS                                            
099300                IF CLAG-PRARTSTD = ZERO                                   
099400                  ADD 1 TO EX                                             
099500                  MOVE 'Price is missing' TO ERROR-LINE(EX)               
099600                END-IF                                                    
099700                MOVE CLAG-ADLAGOMR  TO UT2-ADLAGOMR-CDC                   
099800                MOVE CLAG-ADGANG    TO UT2-ADGANG-CDC                     
099900                MOVE CLAG-ADPLATS   TO UT2-ADPLATS-CDC                    
100000              END-IF                                                      
100100            ELSE                                                          
100200              PERFORM IMS-GU-WDB616                                       
100300              IF SEGMENT-FINNS                                            
100310                IF DCS-IDDISTR-REFILL = ZERO                              
100320                  ADD 1 TO EX                                             
100330                  MOVE 'District zero,look 4404 screen'                   
100340                                                TO ERROR-LINE(EX)         
100350                ELSE                                                      
100400                  MOVE REF-IDDISTR-REFILL TO UT2-IDDISTR                  
100500**                                    so you get dist in errormail        
100600                                             IN-ODTA-IDDISTR              
100610                END-IF                                                    
100700                MOVE W-IDDC-REF       TO W-IDDC                           
100800                PERFORM IMS-GU-WDK611                                     
100900                IF SEGMENT-FINNS                                          
101000                  IF CLAG-PRARTSTD = ZERO                                 
101100                    ADD 1 TO EX                                           
101200                    MOVE 'Price is missing' TO ERROR-LINE(EX)             
101300                  END-IF                                                  
101400                END-IF                                                    
101500                PERFORM IMS-GU-WDK711                                     
101600                IF SEGMENT-FINNS                                          
101700                  MOVE SLAG-ADLAGOMR TO UT2-ADLAGOMR-CDC                  
101800                  MOVE SLAG-ADGANG  TO UT2-ADGANG-CDC                     
101900                  MOVE SLAG-ADPLATS TO UT2-ADPLATS-CDC                    
102000                END-IF                                                    
102100              ELSE                                                        
102200                ADD 1 TO EX                                               
102300                MOVE 'Refill way missing' TO ERROR-LINE(EX)               
102400              END-IF                                                      
102500            END-IF                                                        
102600          ELSE                                                            
102700            ADD 1 TO EX                                                   
102800            MOVE 'Refill way missing' TO ERROR-LINE(EX)                   
102900          END-IF                                                          
103000         END-IF                                                           
103100       ELSE                                                               
103200          ADD 1 TO EX                                                     
103300          MOVE 'Part not on receiving DC'                                 
103400          TO ERROR-LINE(EX)                                               
103600       END-IF                                                             
103700     .                                                                    
103800                                                                          
103900     EJECT                                                                
104000 EB-REFILL-BYPASS-ORDER SECTION.                                          
104100                                                                          
104200***FOR BYPASS REFILL ORDER WE USE DISTRICT COLUMN FOR                     
104300***RECEIVING DC, THAT'S WHY WE MOVE IDDISTR TO IDDC                       
104400     UNSTRING IN-ODTA-IDDISTR DELIMITED BY SPACE                          
104500              INTO W-IDDC                                                 
104600              COUNT IN W-IDDC-LENG                                        
104700     IF W-IDDC-LENG > LENGTH OF W-IDDC                                    
104800        ADD 1 TO EX                                                       
104900        MOVE 'To DC is invalid' TO ERROR-LINE(EX)                         
105000     END-IF                                                               
105010     INSPECT W-IDDC                                                       
105020        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
105100     IF IN-ODTA-FLFORBI = YES AND                                         
105200        W-IDDC = SPACE                                                    
105300        ADD 1 TO EX                                                       
105400        MOVE 'To DC must be given' TO ERROR-LINE(EX)                      
105500     END-IF                                                               
105600                                                                          
105610     PERFORM IMS-GU-WDK711                                                
105620     IF SEGMENT-FINNS                                                     
105630       IF SLAG-IDDC-REF = SPACE                                           
105640        ADD 1 TO EX                                                       
105650        MOVE 'Can not refill to purchase DC'    TO ERROR-LINE(EX)         
105660       END-IF                                                             
105661     ELSE                                                                 
105662        ADD 1 TO EX                                                       
105663        MOVE 'Part missing on To DC'    TO ERROR-LINE(EX)                 
105670     END-IF                                                               
105700     PERFORM IMS-GU-WDB601                                                
105800     IF SEGMENT-FINNS                                                     
105900       IF W-IDDC-REF = '11'                                               
106000         MOVE DCS-IDDISTR-REFILL TO UT-OLIN-IDDISTR                       
106001         IF DCS-IDDISTR-REFILL = ZERO                                     
106002           ADD 1 TO EX                                                    
106003           MOVE 'District zero,look 4404 screen' TO ERROR-LINE(EX)        
106004         ELSE                                                             
106010           IF W-KDORDKL = 1                                               
106100             MOVE DCS-IDKUNDNR-SBPS TO UT-OLIN-IDKUNDNR                   
106101             MOVE DCS-KDFRAKT-SBPS TO UT-OLIN-KDFRAKT                     
106102           ELSE                                                           
106110             MOVE DCS-IDKUNDNR-BPS TO UT-OLIN-IDKUNDNR                    
106111             MOVE DCS-KDFRAKT-BPS TO UT-OLIN-KDFRAKT                      
106120           END-IF                                                         
106130         END-IF                                                           
106200       ELSE                                                               
106300         PERFORM IMS-GU-WDB616                                            
106400         IF SEGMENT-FINNS                                                 
106500           MOVE REF-IDDISTR-REFILL TO UT-OLIN-IDDISTR                     
106501           IF REF-IDDISTR-REFILL = ZERO                                   
106502             ADD 1 TO EX                                                  
106503             MOVE 'District zero look 4404 screen'                        
106504                                      TO ERROR-LINE(EX)                   
106505           ELSE                                                           
106510             IF W-KDORDKL = 1                                             
106600               MOVE REF-IDKUNDNR-SBPS TO UT-OLIN-IDKUNDNR                 
106601               MOVE DCS-KDFRAKT-SBPS TO UT-OLIN-KDFRAKT                   
106610             ELSE                                                         
106611               MOVE REF-IDKUNDNR-BPS TO UT-OLIN-IDKUNDNR                  
106612               MOVE DCS-KDFRAKT-BPS TO UT-OLIN-KDFRAKT                    
106620             END-IF                                                       
106630           END-IF                                                         
106700         ELSE                                                             
106800           ADD 1 TO EX                                                    
106900           MOVE 'Refill way missing/wrong' TO ERROR-LINE(EX)              
107000         END-IF                                                           
107100       END-IF                                                             
107200       PERFORM IMS-GU-WDK611                                              
107300       IF SEGMENT-FINNS                                                   
107400         IF CLAG-PRARTSTD = ZERO                                          
107500           ADD 1 TO EX                                                    
107600           MOVE 'Price is missing' TO ERROR-LINE(EX)                      
107700         END-IF                                                           
107800       END-IF                                                             
107900***uses same idsystem as for 2362 for bypass                              
107910       MOVE ZERO                 TO UT-OLIN-KDTPOTYP                      
107920       MOVE ZERO                 TO UT-OLIN-TITPO                         
107921       MOVE ZERO                 TO UT-OLIN-TIREPDAT                      
107930       MOVE SPACE                TO UT-OLIN-BEGMT-RAD1                    
107940                                    UT-OLIN-BEGMT-RAD2                    
107950                                    UT-OLIN-ADGMT-GATA                    
108000       MOVE W-IDARTNR-2          TO UT-OLIN-IDARTNR                       
108100       MOVE W-REKSIFFR           TO UT-OLIN-REKSIFFR                      
108200       MOVE 'REFB'               TO UT-OLIN-IDSYSTEM                      
108400       MOVE W-KDORDKL            TO UT-OLIN-KDORDKL                       
108500       MOVE YES                  TO UT-OLIN-FLFORBI                       
108610       MOVE W-IDDC               TO UT-OLIN-IDDC                          
108700       MOVE W-KVBEART            TO UT-OLIN-KVBEART                       
108800       MOVE IN-ODTA-BERADREF     TO UT-OLIN-BERADREF                      
108900     ELSE                                                                 
109000       ADD 1 TO EX                                                        
109100       MOVE 'Refill way missing/wrong' TO ERROR-LINE(EX)                  
109200     END-IF                                                               
109300                                                                          
109400                                                                          
109500     .                                                                    
109600                                                                          
109700     EJECT                                                                
109800 F-WRITE-ERROR-MAIL-DATA SECTION.                                         
109900                                                                          
110000     IF NOT EMAIL-HEADER-WRITTEN                                          
110100        SET EMAIL-HEADER-WRITTEN TO TRUE                                  
110200        WRITE EMAIL-RECORD FROM EMAIL-HDR-1                               
110300        WRITE EMAIL-RECORD FROM EMAIL-SPACE                               
110400        WRITE EMAIL-RECORD FROM EMAIL-HDR-2                               
110500        WRITE EMAIL-RECORD FROM EMAIL-HDR-2B                              
110600        WRITE EMAIL-RECORD FROM EMAIL-SPACE                               
110700        IF W-IDSYSTEM = SPX-IDSYSTEM                                      
110800          MOVE W-BEMARKN  TO EMAIL-BEMARKN                                
110900          WRITE EMAIL-RECORD FROM EMAIL-HDR-3A                            
111000        ELSE                                                              
111100          IF W-IDSYSTEM NOT = REFILL-IDSYSTEM                             
111200            MOVE W-BEKUNDRF TO EMAIL-BEKUNDRF                             
111300            WRITE EMAIL-RECORD FROM EMAIL-HDR-3B                          
111400          END-IF                                                          
111500        END-IF                                                            
111600        WRITE EMAIL-RECORD FROM EMAIL-HDR-4                               
111700        WRITE EMAIL-RECORD FROM EMAIL-SPACE                               
111800        IF (W-IDSYSTEM = REFILL-IDSYSTEM)                                 
111900        AND W-FLFORBI = 'N'                                               
112000******WHEN CREATING REFILL APPROVED PROPOSALS,SHOW                        
112100******RECEIVING DC INSTEAD OF DISTRICT                                    
112200          MOVE 'DC        ' TO EMAIL-COL1                                 
112300          MOVE '          ' TO EMAIL-COL2                                 
112400        ELSE                                                              
112500          MOVE 'District  ' TO EMAIL-COL1                                 
112600          MOVE 'Dealer    ' TO EMAIL-COL2                                 
112700        END-IF                                                            
112800        WRITE EMAIL-RECORD FROM EMAIL-HDR-5                               
112900     END-IF                                                               
113000                                                                          
113100     WRITE EMAIL-RECORD FROM EMAIL-SPACE                                  
113200     IF (W-IDSYSTEM = REFILL-IDSYSTEM)                                    
113300     AND W-FLFORBI = 'N'                                                  
113400******WHEN CREATING REFILL APPROVED PROPOSALS,SHOW                        
113500******RECEIVING DC INSTEAD OF DISTRICT                                    
113600       MOVE IN-ODTA-IDDC    TO EMAIL-IDDISTR                              
113700     ELSE                                                                 
113800       MOVE IN-ODTA-IDDISTR TO EMAIL-IDDISTR                              
113900     END-IF                                                               
114000     MOVE IN-ODTA-IDKUNDNR TO EMAIL-IDKUNDNR                              
114100     IF W-IDARTNR = 0                                                     
114200       MOVE ZERO           TO EMAIL-IDARTNR                               
114300     ELSE                                                                 
114400       MOVE IN-ODTA-IDARTNR TO EMAIL-IDARTNR                              
114500     END-IF                                                               
114600     MOVE IN-ODTA-KVBEART  TO EMAIL-KVBEART                               
114700     WRITE EMAIL-RECORD FROM EMAIL-DATA                                   
114800                                                                          
114900     MOVE 1 TO IX                                                         
115000     PERFORM UNTIL IX > EX                                                
115100       MOVE ERROR-LINE(IX)  TO EMAIL-ERROR-TEXT                           
115200       WRITE EMAIL-RECORD FROM EMAIL-ERROR-LINE                           
115300       ADD 1 TO IX                                                        
115400     END-PERFORM                                                          
115500     .                                                                    
115600                                                                          
115700     EJECT                                                                
115800 Z-FINIT SECTION.                                                         
115900                                                                          
116000     CLOSE INPARM W41282 W41283 W4128E W41285                             
116100                                                                          
116200     MOVE 'S' TO POSTSUM-OPKOD                                            
116300     CALL POSTSUM USING POSTSUM-PARM                                      
116400     .                                                                    
116500                                                                          
116600     EJECT                                                                
116700 S01-READ-W41282  SECTION.                                                
116800                                                                          
116900     READ W41282 INTO IN-AREA                                             
117000     AT END                                                               
117100        MOVE HIGH-VALUE TO IN-AREA                                        
117200        SET END-OF-W41282 TO TRUE                                         
117300                                                                          
117400     NOT AT END                                                           
117500        MOVE 'W41282'   TO POSTSUM-FDNAMN                                 
117600        MOVE 'W41283D1' TO POSTSUM-DDNAMN2                                
117700        MOVE SPACE      TO POSTSUM-TRANSTYP                               
117800        CALL POSTSUM USING POSTSUM-PARM                                   
117900     END-READ                                                             
118000     .                                                                    
118100                                                                          
118200     EJECT                                                                
118300 S11-WRITE-W41283 SECTION.                                                
118400                                                                          
118500     WRITE UT-RECORD FROM UT-AREA                                         
118600                                                                          
118700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
118800     MOVE 'W41283'   TO POSTSUM-FDNAMN                                    
118900     MOVE 'W41283D2' TO POSTSUM-DDNAMN2                                   
119000     CALL POSTSUM USING POSTSUM-PARM                                      
119100     .                                                                    
119200     EJECT                                                                
119300                                                                          
119400 S12-WRITE-W41285 SECTION.                                                
119500                                                                          
119600     WRITE UT2-POST FROM UT2-AREA                                         
119700                                                                          
119800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
119900     MOVE 'W41285'   TO POSTSUM-FDNAMN                                    
120000     MOVE 'W41283D5' TO POSTSUM-DDNAMN2                                   
120100     CALL POSTSUM USING POSTSUM-PARM                                      
120200     .                                                                    
120300     EJECT                                                                
120400                                                                          
120500*****************IMS READINGS*****************                            
120600 IMS-GU-WDK711         SECTION.                                           
120700                                                                          
120800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-2-X ')'                       
120900          DELIMITED BY SIZE INTO SSA1                                     
121000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
121100          DELIMITED BY SIZE INTO SSA2                                     
121200     MOVE '  GE'                TO GODK-STATUSKODER                       
121300     CALL CBLTDLI USING GU                                                
121400                        ARTS-PCB                                          
121500                        IO-AREA-K711                                      
121600                        SSA1                                              
121700                        SSA2                                              
121800     MOVE ARTS-STATUS-CODE      TO STATUS-WS                              
121900     PERFORM IMS-STATUSKONTROLL                                           
122000     .                                                                    
122100 IMS-GU-WDB601    SECTION.                                                
122200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
122300          DELIMITED BY SIZE INTO SSA1                                     
122400     MOVE '  GE' TO GODK-STATUSKODER                                      
122500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
122600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     .                                                                    
122900     EJECT                                                                
123000 IMS-GU-WDB616    SECTION.                                                
123100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
123200          DELIMITED BY SIZE INTO SSA1                                     
123300     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
123400          DELIMITED BY SIZE INTO SSA2                                     
123500     MOVE '  GE' TO GODK-STATUSKODER                                      
123600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
123700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
123800     PERFORM IMS-STATUSKONTROLL                                           
123900     .                                                                    
124000     EJECT                                                                
124100                                                                          
124200 IMS-GU-WDK611 SECTION.                                                   
124300                                                                          
124400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-2-X ')'                       
124500          DELIMITED BY SIZE INTO SSA1                                     
124600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
124700          DELIMITED BY SIZE INTO SSA2                                     
124800     MOVE '  GE' TO GODK-STATUSKODER                                      
124900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
125000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
125100     PERFORM IMS-STATUSKONTROLL                                           
125200     .                                                                    
125300     EJECT                                                                
125400                                                                          
125500 IMS-STATUSKONTROLL SECTION.                                              
125600                                                                          
125700     SET STATUS-IX TO 1                                                   
125800     SEARCH GODK-STATUS                                                   
125900       AT END CALL FELLOG                                                 
126000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
126100     END-SEARCH                                                           
126200     .                                                                    
126300                                                                          
126400*S99-ABEND SECTION.                                                       
126500*                                                                         
126600*    MOVE 'S' TO POSTSUM-OPKOD                                            
126700*    CALL POSTSUM USING POSTSUM-PARM                                      
126800*    CALL ABEND USING RKOD-ABEND-NO-DUMP                                  
126900*    .                                                                    
