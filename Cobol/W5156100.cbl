000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5156100.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20170822.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER                                                 
000900*        1 EKONOMISK HÄNDELSEFIL                                          
001000*                                                                         
001100*        OCH SKAPAR                                                       
001200*        2 FIL TILL KONTROLL                                              
001300*                                                                         
001400     EJECT                                                                
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INFIL EKONOMI                                              
002300     SELECT W51561                     ASSIGN TO W51561D1.                
002400     SKIP2                                                                
002500*          --- INFIL EKONOMI                                              
002600     SELECT W5156A                     ASSIGN TO W51561D2.                
002700     SKIP2                                                                
002800*          --- UTFIL KONTROLL                                             
002900     SELECT W51562                     ASSIGN TO W51561D3.                
003000     SKIP2                                                                
003100*          --- UTFIL KONTROLL                                             
003200     SELECT W5156B                     ASSIGN TO W51561D4.                
003300     SKIP2                                                                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W51561                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200 01  IN-POST.                                                             
004300*    03   -COPY W51560    -L.                                             
004400                                                                          
004500 FD  W5156A                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800 01  IN2-POST.                                                            
004900*    03   -COPY W51560    -L.                                             
005000                                                                          
005100 FD  W51562                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400*01  POST -COPY WDR801 -PRE  UT1-  -L.                                    
005500                                                                          
005600 FD  W5156B                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900*01  POST -COPY WDR801 -PRE  UT2-  -L.                                    
006000                                                                          
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300 77  IDPGM                       PIC X(8)    VALUE 'W5156100'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  W51561-EOF-SW               PIC X       VALUE 'N'.                   
006700     88  END-OF-W51561                       VALUE 'J'.                   
006800 77  W5156A-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W5156A                       VALUE 'J'.                   
007000                                                                          
007100 01  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300     EJECT                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007900                                                                          
008000*    --- PARAMETRAR TILL ABEND                                            
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008400     SKIP2                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL DATKORT                                          
009000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009100                                                                          
009200*01  -COPY WDATKORT                                                       
009300     EJECT                                                                
009400                                                                          
009500*    --- PARAMETRAR TILL POSTSUM                                          
009600*                                                                         
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800     EJECT                                                                
009900                                                                          
010000 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
010100*    --- INAREA EKONOMIPOST                                               
010200*01  AREA -COPY W51560  -PRE IN-                                          
010300     EJECT                                                                
010400                                                                          
010500 01  FILLER                      PIC X(24)   VALUE 'IN2-AREA'.            
010600*    --- INAREA EKONOMIPOST                                               
010700*01  AREA -COPY W51560  -PRE IN2-                                         
010800     EJECT                                                                
010900                                                                          
011000 01  FILLER                      PIC X(24)   VALUE 'UT1-AREA'.            
011100*01  AREA -COPY WDR801     -PRE UT1-                                      
011200*    05   -COPY W510EKHA   -PRE UT1- -RED UT1-FIL-WDR801-DATA             
011300     EJECT                                                                
011400                                                                          
011500 01  FILLER                      PIC X(24)   VALUE 'UT2-AREA'.            
011600*01  AREA -COPY WDR801     -PRE UT2-                                      
011700*    05   -COPY W510EKHA   -PRE UT2- -RED UT2-FIL-WDR801-DATA             
011800     EJECT                                                                
011900                                                                          
014400     EJECT                                                                
014500                                                                          
014600 LINKAGE SECTION.                                                         
014700 PROCEDURE DIVISION.                                                      
014800 MAIN SECTION.                                                            
014900                                                                          
015000     PERFORM A-INIT                                                       
015100                                                                          
015200     PERFORM S01-LAES-W51561                                              
015300     PERFORM UNTIL END-OF-W51561                                          
015400       PERFORM D1-FLYTTA-POST-TILL-UTAREA1                                
015500       PERFORM S01-LAES-W51561                                            
015600     END-PERFORM                                                          
015700                                                                          
015800     PERFORM S01-LAES-W5156A                                              
015900     PERFORM UNTIL END-OF-W5156A                                          
016000       PERFORM D1-FLYTTA-POST-TILL-UTAREA2                                
016100       PERFORM S01-LAES-W5156A                                            
016200     END-PERFORM                                                          
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000                                                                          
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     OPEN INPUT  W51561                                                   
017400     OPEN INPUT  W5156A                                                   
017500     OPEN OUTPUT W51562                                                   
017600     OPEN OUTPUT W5156B                                                   
017700                                                                          
017800     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
017900                                                                          
018000     MOVE FUNCTION CURRENT-DATE(1:4)   TO DAGENS-AAR                      
018100     MOVE FUNCTION CURRENT-DATE(3:6)   TO DAGENS-DATUM                    
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300     .                                                                    
018400     EJECT                                                                
018500                                                                          
018600 D1-FLYTTA-POST-TILL-UTAREA1 SECTION.                                     
018700     MOVE IN-EKHT-IDPGM      TO UT1-FIL-IDPGM                             
018800     MOVE IN-EKHT-TIREGDAT   TO UT1-FIL-TIREGDAT                          
018900     MOVE IN-EKHT-TIKLOCK    TO UT1-FIL-TIKLOCK                           
019000     MOVE IN-EKHT-IDSEKVNR   TO UT1-FIL-IDSEKVNR                          
019100     MOVE 'W515EKHA'         TO UT1-FIL-IDCPYTXT                          
019200     MOVE IN-EKHT-BEVAT      TO UT1-EKH-BEVAT                             
019300     MOVE IN-EKHT-DAVERDAT   TO UT1-EKH-DAVERDAT                          
019400     MOVE IN-EKHT-FLLSBOK    TO UT1-EKH-FLLSBOK                           
019500     MOVE IN-EKHT-IDANALYS   TO UT1-EKH-IDANALYS                          
019600     MOVE IN-EKHT-IDARTNR    TO UT1-EKH-IDARTNR                           
019700     MOVE IN-EKHT-IDDC-SEND  TO UT1-EKH-IDDC-SEND                         
019800     MOVE IN-EKHT-IDDC-REC   TO UT1-EKH-IDDC-REC                          
019900     MOVE IN-EKHT-IDDISTR    TO UT1-EKH-IDDISTR                           
020000     MOVE IN-EKHT-IDKONTO    TO UT1-EKH-IDKONTO                           
020100     MOVE IN-EKHT-IDKST      TO UT1-EKH-IDKST                             
020200     MOVE IN-EKHT-IDKUNDNR   TO UT1-EKH-IDKUNDNR                          
020300     MOVE IN-EKHT-IDTRANS    TO UT1-EKH-IDTRANS                           
020400     MOVE IN-EKHT-IDVERGL    TO UT1-EKH-IDVERGL                           
020500     MOVE IN-EKHT-KDANMORS   TO UT1-EKH-KDANMORS                          
020600     MOVE IN-EKHT-KDEKHHT    TO UT1-EKH-KDEKHHT                           
020700     MOVE IN-EKHT-KDEKSHT    TO UT1-EKH-KDEKSHT                           
020800     MOVE IN-EKHT-KDEKNIVA   TO UT1-EKH-KDEKNIVA                          
020900     MOVE IN-EKHT-KDFRAKT    TO UT1-EKH-KDFRAKT                           
021000     MOVE IN-EKHT-KDPRODSL   TO UT1-EKH-KDPRODSL                          
021100     MOVE IN-EKHT-KDPSLLOC   TO UT1-EKH-KDPSLLOC                          
021200     MOVE IN-EKHT-KDVALISO   TO UT1-EKH-KDVALISO                          
021300     MOVE IN-EKHT-KVANTAL    TO UT1-EKH-KVANTAL                           
021400     MOVE IN-EKHT-PRARTNTO   TO UT1-EKH-PRARTNTO                          
021500     MOVE IN-EKHT-PRARTSJK   TO UT1-EKH-PRARTSJK                          
021600     MOVE IN-EKHT-PRHEMTAG   TO UT1-EKH-PRHEMTAG                          
021700     MOVE IN-EKHT-PRARTSTD   TO UT1-EKH-PRARTSTD                          
021800     MOVE IN-EKHT-PRDIRLON   TO UT1-EKH-PRDIRLON                          
021900     MOVE IN-EKHT-PRDMTRL    TO UT1-EKH-PRDMTRL                           
022000     MOVE IN-EKHT-PRINK      TO UT1-EKH-PRINK                             
022100     MOVE IN-EKHT-PRKURS     TO UT1-EKH-PRKURS                            
022200     MOVE IN-EKHT-PRLANDCO   TO UT1-EKH-PRLANDCO                          
022300     MOVE IN-EKHT-PROVRPAL   TO UT1-EKH-PROVRPAL                          
022400     MOVE IN-EKHT-SUBEL      TO UT1-EKH-SUBEL                             
022500     MOVE IN-EKHT-SUVAT      TO UT1-EKH-SUVAT                             
022600     MOVE IN-EKHT-DAAVIDAT   TO UT1-EKH-DAAVIDAT                          
022700     MOVE IN-EKHT-IDAVINR    TO UT1-EKH-IDAVINR                           
022800     MOVE IN-EKHT-IDLEVNR    TO UT1-EKH-IDLEVNR                           
022900     MOVE IN-EKHT-KDAVVTYP   TO UT1-EKH-KDAVVTYP                          
023000     MOVE IN-EKHT-KDRT       TO UT1-EKH-KDRT                              
023100     MOVE IN-EKHT-KVANTMOT   TO UT1-EKH-KVANTMOT                          
023200     MOVE IN-EKHT-KVAVIS     TO UT1-EKH-KVAVIS                            
023300     MOVE IN-EKHT-KDSORT     TO UT1-EKH-KDSORT                            
023400     MOVE IN-EKHT-KDTRADP    TO UT1-EKH-KDTRADP                           
023500     MOVE IN-EKHT-FLOVRLEV   TO UT1-EKH-FLOVRLEV                          
023600     IF IN-EKHT-FLDCET = JA                                               
023700       MOVE JA               TO UT1-EKH-FLDCET                            
023800     ELSE                                                                 
023900       MOVE NEJ              TO UT1-EKH-FLDCET                            
024000     END-IF                                                               
024100     IF IN-EKHT-IDORDNR5 NUMERIC                                          
024200       MOVE IN-EKHT-IDORDNR5 TO UT1-EKH-IDORDNR5                          
024300     ELSE                                                                 
024400       MOVE ZERO             TO UT1-EKH-IDORDNR5                          
024500     END-IF                                                               
024600     MOVE IN-EKHT-IDUSER     TO UT1-EKH-IDUSER                            
024700     MOVE SPACE              TO UT1-EKH-IDREF                             
024800                                UT1-EKH-BEFELSAP                          
024900     MOVE IN-EKHT-IDKUNDRF   TO UT1-EKH-IDKUNDRF                          
024910     MOVE IN-EKHT-IDFAKT-EXP TO UT1-EKH-IDFAKT-EXP                        
025000     MOVE 'Y'                TO UT1-EKH-FLKLAR                            
025100     PERFORM S11-SKRIV-W51562                                             
025200     .                                                                    
025300     EJECT                                                                
025400                                                                          
025500 D1-FLYTTA-POST-TILL-UTAREA2 SECTION.                                     
025600     MOVE IN2-EKHT-IDPGM      TO UT2-FIL-IDPGM                            
025700     MOVE IN2-EKHT-TIREGDAT   TO UT2-FIL-TIREGDAT                         
025800     MOVE IN2-EKHT-TIKLOCK    TO UT2-FIL-TIKLOCK                          
025900     MOVE IN2-EKHT-IDSEKVNR   TO UT2-FIL-IDSEKVNR                         
026000     MOVE 'W515EKHB'         TO UT2-FIL-IDCPYTXT                          
026100     MOVE IN2-EKHT-BEVAT      TO UT2-EKH-BEVAT                            
026200     MOVE IN2-EKHT-DAVERDAT   TO UT2-EKH-DAVERDAT                         
026300     MOVE IN2-EKHT-FLLSBOK    TO UT2-EKH-FLLSBOK                          
026400     MOVE IN2-EKHT-IDANALYS   TO UT2-EKH-IDANALYS                         
026500     MOVE IN2-EKHT-IDARTNR    TO UT2-EKH-IDARTNR                          
026600     MOVE IN2-EKHT-IDDC-SEND  TO UT2-EKH-IDDC-SEND                        
026700     MOVE IN2-EKHT-IDDC-REC   TO UT2-EKH-IDDC-REC                         
026800     MOVE IN2-EKHT-IDDISTR    TO UT2-EKH-IDDISTR                          
026900     MOVE IN2-EKHT-IDKONTO    TO UT2-EKH-IDKONTO                          
027000     MOVE IN2-EKHT-IDKST      TO UT2-EKH-IDKST                            
027100     MOVE IN2-EKHT-IDKUNDNR   TO UT2-EKH-IDKUNDNR                         
027200     MOVE IN2-EKHT-IDTRANS    TO UT2-EKH-IDTRANS                          
027300     MOVE IN2-EKHT-IDVERGL    TO UT2-EKH-IDVERGL                          
027400     MOVE IN2-EKHT-KDANMORS   TO UT2-EKH-KDANMORS                         
027500     MOVE IN2-EKHT-KDEKHHT    TO UT2-EKH-KDEKHHT                          
027600     MOVE IN2-EKHT-KDEKSHT    TO UT2-EKH-KDEKSHT                          
027700     MOVE IN2-EKHT-KDEKNIVA   TO UT2-EKH-KDEKNIVA                         
027800     MOVE IN2-EKHT-KDFRAKT    TO UT2-EKH-KDFRAKT                          
027900     MOVE IN2-EKHT-KDPRODSL   TO UT2-EKH-KDPRODSL                         
028000     MOVE IN2-EKHT-KDPSLLOC   TO UT2-EKH-KDPSLLOC                         
028100     MOVE IN2-EKHT-KDVALISO   TO UT2-EKH-KDVALISO                         
028200     MOVE IN2-EKHT-KVANTAL    TO UT2-EKH-KVANTAL                          
028300     MOVE IN2-EKHT-PRARTNTO   TO UT2-EKH-PRARTNTO                         
028400     MOVE IN2-EKHT-PRARTSJK   TO UT2-EKH-PRARTSJK                         
028500     MOVE IN2-EKHT-PRHEMTAG   TO UT2-EKH-PRHEMTAG                         
028600     MOVE IN2-EKHT-PRARTSTD   TO UT2-EKH-PRARTSTD                         
028700     MOVE IN2-EKHT-PRDIRLON   TO UT2-EKH-PRDIRLON                         
028800     MOVE IN2-EKHT-PRDMTRL    TO UT2-EKH-PRDMTRL                          
028900     MOVE IN2-EKHT-PRINK      TO UT2-EKH-PRINK                            
029000     MOVE IN2-EKHT-PRKURS     TO UT2-EKH-PRKURS                           
029100     MOVE IN2-EKHT-PRLANDCO   TO UT2-EKH-PRLANDCO                         
029200     MOVE IN2-EKHT-PROVRPAL   TO UT2-EKH-PROVRPAL                         
029300     MOVE IN2-EKHT-SUBEL      TO UT2-EKH-SUBEL                            
029400     MOVE IN2-EKHT-SUVAT      TO UT2-EKH-SUVAT                            
029500     MOVE IN2-EKHT-DAAVIDAT   TO UT2-EKH-DAAVIDAT                         
029600     MOVE IN2-EKHT-IDAVINR    TO UT2-EKH-IDAVINR                          
029700     MOVE IN2-EKHT-IDLEVNR    TO UT2-EKH-IDLEVNR                          
029800     MOVE IN2-EKHT-KDAVVTYP   TO UT2-EKH-KDAVVTYP                         
029900     MOVE IN2-EKHT-KDRT       TO UT2-EKH-KDRT                             
030000     MOVE IN2-EKHT-KVANTMOT   TO UT2-EKH-KVANTMOT                         
030100     MOVE IN2-EKHT-KVAVIS     TO UT2-EKH-KVAVIS                           
030200     MOVE IN2-EKHT-KDSORT     TO UT2-EKH-KDSORT                           
030300     MOVE IN2-EKHT-KDTRADP    TO UT2-EKH-KDTRADP                          
030400     MOVE IN2-EKHT-FLOVRLEV   TO UT2-EKH-FLOVRLEV                         
030500     IF IN2-EKHT-FLDCET = JA                                              
030600       MOVE JA               TO UT2-EKH-FLDCET                            
030700     ELSE                                                                 
030800       MOVE NEJ              TO UT2-EKH-FLDCET                            
030900     END-IF                                                               
031000     IF IN2-EKHT-IDORDNR5 NUMERIC                                         
031100       MOVE IN2-EKHT-IDORDNR5 TO UT2-EKH-IDORDNR5                         
031200     ELSE                                                                 
031300       MOVE ZERO             TO UT2-EKH-IDORDNR5                          
031400     END-IF                                                               
031500     MOVE IN2-EKHT-IDUSER    TO UT2-EKH-IDUSER                            
031600     MOVE SPACE              TO UT2-EKH-IDREF                             
031700                                UT2-EKH-BEFELSAP                          
031800     MOVE IN2-EKHT-IDKUNDRF  TO UT2-EKH-IDKUNDRF                          
031810     MOVE IN2-EKHT-IDFAKT-EXP                                             
031820                             TO UT2-EKH-IDFAKT-EXP                        
031900     MOVE 'Y'                TO UT2-EKH-FLKLAR                            
032000     PERFORM S11-SKRIV-W5156B                                             
032100     .                                                                    
032200     EJECT                                                                
032300                                                                          
032400 Z-FINIT SECTION.                                                         
032500     CLOSE W51561                                                         
032600           W51562                                                         
032700           W5156A                                                         
032800           W5156B                                                         
032900     SKIP2                                                                
033000     MOVE 'S' TO POSTSUM-OPKOD                                            
033100     CALL POSTSUM USING POSTSUM-PARM                                      
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033500 S01-LAES-W51561  SECTION.                                                
033600     READ W51561          INTO IN-AREA                                    
033700     AT END                                                               
033800        MOVE HIGH-VALUE   TO IN-AREA                                      
033900        SET END-OF-W51561 TO TRUE                                         
034000     NOT AT END                                                           
034100        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
034200        MOVE 'W51561'     TO POSTSUM-FDNAMN                               
034300        MOVE 'W51561D1'   TO POSTSUM-DDNAMN2                              
034400        CALL POSTSUM USING POSTSUM-PARM                                   
034500     END-READ                                                             
034600     .                                                                    
034700                                                                          
034800 S01-LAES-W5156A  SECTION.                                                
034900     READ W5156A          INTO IN2-AREA                                   
035000     AT END                                                               
035100        MOVE HIGH-VALUE   TO IN2-AREA                                     
035200        SET END-OF-W5156A TO TRUE                                         
035300     NOT AT END                                                           
035400        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
035500        MOVE 'W5156A'     TO POSTSUM-FDNAMN                               
035600        MOVE 'W51561D2'   TO POSTSUM-DDNAMN2                              
035700        CALL POSTSUM USING POSTSUM-PARM                                   
035800     END-READ                                                             
035900     .                                                                    
036000                                                                          
036100 S11-SKRIV-W51562 SECTION.                                                
036200     WRITE UT1-POST  FROM UT1-AREA                                        
036300                                                                          
036400     MOVE 'UT1-'     TO POSTSUM-TRANSTYP                                  
036500     MOVE 'W51562'   TO POSTSUM-FDNAMN                                    
036600     MOVE 'W51561D2' TO POSTSUM-DDNAMN2                                   
036700     CALL POSTSUM USING POSTSUM-PARM                                      
036800     .                                                                    
036900     EJECT                                                                
037000                                                                          
037100 S11-SKRIV-W5156B SECTION.                                                
037200     WRITE UT2-POST  FROM UT2-AREA                                        
037300                                                                          
037400     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
037500     MOVE 'W5156B'   TO POSTSUM-FDNAMN                                    
037600     MOVE 'W5156BD2' TO POSTSUM-DDNAMN2                                   
037700     CALL POSTSUM USING POSTSUM-PARM                                      
037800     .                                                                    
037900     EJECT                                                                
