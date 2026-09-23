000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9042100.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   SEPTEMBER 1990.                                          
000500 DATE-COMPILED.                                                           
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        FRÅGEPROGRAM, ARTIKELFRÅGA I KATALOG                             
001000*-----------------------------------------------------------              
001100*    ÄNDRING.                                                             
001200*        TILLAGT VISNING AV MASTERNAMN.                                   
001300*        ÄTNR K91. 910314. NÄSTA KATALOG SKALL VISAS VID FORTSATT         
001400*                          TRYCK PÅ PF8.                                  
001410*        990901  LAGT TILL ACCESS AV USER-BASEN  /CE                      
001420*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W90421t                                             
001700*        MID:         W90421I1                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W90421O1                                            
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                   PIC X(8)    VALUE 'W9042100'.                
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 01  GENERELLA-SUBPROGRAM.                                                
003400   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
003500   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
003600   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
003700   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
003800   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
003900   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
004000                                                                          
004100     EJECT                                                                
004200*01  -COPY WDATAREA                                                       
004300     SKIP2                                                                
004400 77  JA                      PIC X       VALUE 'J'.                       
004500 77  NEJ                     PIC X       VALUE 'N'.                       
004600 77  WS-KVKOL                PIC X(3)    VALUE '-  '.                     
004700 01  WS-KDCATPUB-R-AVV       PIC X(3)    VALUE SPACE.                     
004800 01  WS-KDCATPUB-AAAAVV      PIC X(6)    VALUE SPACE.                     
004900 77  SPRAK-IX                PIC S9(9)   VALUE +1   COMP SYNC.            
005000 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
005100 77  R-IX                    PIC S9(9)   VALUE +1   COMP SYNC.            
005200 77  RAD-MAX                 PIC S9(3)   VALUE +13  COMP SYNC.            
005300 77  K-IX                    PIC S9(9)   VALUE +1   COMP SYNC.            
005400 77  KOL-MAX                 PIC S9(9)   VALUE +3   COMP SYNC.            
005500 77  KVANT-IX                PIC S9(9)   VALUE +1   COMP SYNC.            
005600 77  KVANT-MAX               PIC S9(9)   VALUE +5   COMP SYNC.            
005800 77  IDARTNR-WS              PIC S9(9)   VALUE ZERO.                      
005900 77  IDKATNR-WS              PIC S9(5)   VALUE ZERO.                      
006000 77  CURR-IDKATNR-WS         PIC S9(5)   VALUE ZERO.                      
006100 77  SIDRAEK-WS              PIC 9       VALUE 1.                         
006200 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
006400                                                                          
006500 01  WS-GILTIGA-AAR.                                                      
006600   03 WS-TIAAAA                  PIC 9(4)    VALUE ZERO                   
006700                                 OCCURS 4.                                
006800                                                                          
006900                                                                          
007000 77  INDATA-SW               PIC X       VALUE 'J'.                       
007100   88  INDATA-OK                         VALUE 'J'.                       
007200   88  INDATA-FEL                        VALUE 'N'.                       
007300                                                                          
007400 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
007500   88  NYCKLAR-OK                        VALUE 'J'.                       
007600   88  NYCKLAR-FEL                       VALUE 'N'.                       
007700                                                                          
007800 77  ALLT-SW                 PIC X       VALUE 'J'.                       
007900   88  ALLT-OK                           VALUE 'J'.                       
008000                                                                          
008100 77  W-IDTRANS               PIC X(4)  VALUE SPACE.                       
008200   88  EGEN-MID                        VALUE '9421'.                      
008300   88  GODK-MID                        VALUE '9421'                       
008400                                             '1542'                       
008500          '1101' '1102' '1103' '1116' '1117' '1118'                       
008501                               '2101' '2102' '2103'                       
008502                        '2104' '2105' '2106' '2107'.                      
008510   88  HELP-MID                        VALUE '0551'.                      
008600     EJECT                                                                
008700******************************************************************        
008800*    TEXTAREA              ***************************************        
008900 01  TEXT1.                                                               
009000   03  FILLER      PIC X(25) VALUE '* BENÄMNING EJ REG *     '.           
009100   03  FILLER      PIC X(25) VALUE '* DESCR. NOT REGISTERED *'.           
009200 01  FILLER REDEFINES TEXT1.                                              
009300   03  BEN-SAKNAS  PIC X(25) OCCURS 2.                                    
009400*                                                                         
009500 01  TEXT2.                                                               
009600   03  FILLER      PIC X(25) VALUE '* ARTIKEL EJ REG  *      '.           
009700   03  FILLER      PIC X(25) VALUE '* PART NOT REGISTERED *  '.           
009800 01  FILLER REDEFINES TEXT2.                                              
009900   03  ART-SAKNAS  PIC X(25) OCCURS 2.                                    
010000******************************************************************        
010100*    FELTEXTAREA           ***************************************        
010200 01  FEL-1.                                                               
010300   03  FILLER      PIC X(40) VALUE 'UPPLYSTA FÄLT FEL        '.           
010400   03  FILLER      PIC X(40) VALUE 'HIGHLIGHTED FIELDS WRONG '.           
010500 01  FILLER REDEFINES FEL-1.                                              
010600   03  FEL1        PIC X(40) OCCURS 2.                                    
010700 01  FEL-2.                                                               
010800   03  FILLER      PIC X(40) VALUE                                        
010900                     'INGEN TRÄFF PÅ GIVET SÖKVÄRDE   '.                  
011000   03  FILLER      PIC X(40) VALUE                                        
011100                     'NO HIT IN GIVEN SEARCH ARGUMENT '.                  
011200 01  FILLER REDEFINES FEL-2.                                              
011300   03  FEL2        PIC X(40) OCCURS 2.                                    
011400 01  FEL-3.                                                               
011500   03  FILLER      PIC X(40) VALUE 'NYCKLAR FEL              '.           
011600   03  FILLER      PIC X(40) VALUE 'WRONG KEYS               '.           
011700 01  FILLER REDEFINES FEL-3.                                              
011800   03  FEL3        PIC X(40) OCCURS 2.                                    
011900******************************************************************        
012000*    MEDTEXTAREA           ***************************************        
012100 01  MED-1.                                                               
012200   03  FILLER      PIC X(40) VALUE 'FLER RADER FINNS         '.           
012300   03  FILLER      PIC X(40) VALUE 'MORE LINES EXIST         '.           
012400 01  FILLER REDEFINES MED-1.                                              
012500   03  MED1        PIC X(40) OCCURS 2.                                    
012600 01  MED-2.                                                               
012700   03  FILLER      PIC X(40) VALUE 'DETTA ÄR FÖRSTA SIDAN    '.           
012800   03  FILLER      PIC X(40) VALUE 'FIRST PAGE IS SHOWN      '.           
012900 01  FILLER REDEFINES MED-2.                                              
013000   03  MED2        PIC X(40) OCCURS 2.                                    
013100 01  MED-3.                                                               
013200   03  FILLER      PIC X(40) VALUE 'FLER kataloger finns ! '.             
013300   03  FILLER      PIC X(40) VALUE 'MORE catalogs exist !  '.             
013400 01  FILLER REDEFINES MED-3.                                              
013500   03  MED3        PIC X(40) OCCURS 2.                                    
013600 01  MED-4.                                                               
013700   03  FILLER      PIC X(40) VALUE 'Finns EJ FLER kataloger !'.           
013800   03  FILLER      PIC X(40) VALUE 'NO MORE catalogs exist ! '.           
013900 01  FILLER REDEFINES MED-4.                                              
014000   03  MED4        PIC X(40) OCCURS 2.                                    
014100 01  MED-5.                                                               
014200   03  FILLER PIC X(40) VALUE 'Fordonsnyckel satt till  * = ALLA'.        
014300   03  FILLER PIC X(40) VALUE 'Vehicle code set to  * = ALL'.             
014400 01  FILLER REDEFINES MED-5.                                              
014500   03  MED5   PIC X(40) OCCURS 2.                                         
014600******************************************************************        
014700     EJECT                                                                
014800*   -COPY WDECAREA                                                        
014900     EJECT                                                                
015000*   -COPY WMEDAREA                                                        
015100     EJECT                                                                
015200******************************************************************        
015300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015400 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
015500     SKIP3                                                                
015600*01  MID -COPY W90421I1                                                   
015700     EJECT                                                                
015800*01  -COPY WMSGAREA                                                       
015900     EJECT                                                                
016000*  03  MOD -COPY W90421O1 -RED MSG-AREA.                                  
016100     EJECT                                                                
016200*01  -COPY WMFSAREA                                                       
016300     EJECT                                                                
016310*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016320*                                                                         
016330 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016500*01  -COPY WMSGINIT                                                       
016600     EJECT                                                                
016610*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
016620*                                                                         
016630 01  SPAR-AREA.                                                           
016640     03  SPAR-IDTRANS           PIC X(4)    VALUE '9421'.                 
016650     03  SPAR-IDCATNR-ENTER     PIC 9(5).                                 
016660     03  SPAR-IDCATNR-NEXT      PIC 9(5).                                 
016661     03  SPAR-IDCATGRP-NEXT     PIC 9(3).                                 
016662     03  SPAR-IDCATAVS-NEXT     PIC 9(4).                                 
016663     03  SPAR-IDCATRAD-NEXT     PIC 9(4).                                 
016664     03  SPAR-KDCATPUB-R-NEXT   PIC X(3).                                 
016670     EJECT                                                                
016700******************************************************************        
016800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016900 01  IMS-WS.                                                              
017000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
017100     SKIP3                                                                
017200 01  NYCKLAR-TILL-DLI.                                                    
017300   03  W-IDSKYLT-X.                                                       
017400     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
017500*                                                                         
017600*- - - - - - - - - - NYCKLAR FÖR LÄSNING AV WLKATJ01                      
017700   03  W-IDARTNR-X.                                                       
017800     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
017900*                                                                         
018000   03  W-IDCATNR-XL.                                                      
018100     05  W-IDCATNR-L         PIC 9(5)    VALUE ZERO.                      
018200   03  W-IDCATNR-XH.                                                      
018300     05  W-IDCATNR-H         PIC 9(5)    VALUE ZERO.                      
018400*                                                                         
018500   03  W-WDN5B1-LO-X.                                                     
018600     05 W-IDCATGRP-LO        PIC 9(2)    VALUE ZERO.                      
018700     05 W-IDCATAVS-LO        PIC 9(4)    VALUE ZERO.                      
018800     05 W-IDCATRAD-LO        PIC 9(4)    VALUE ZERO.                      
018900     05 W-KDCATPUB-LO        PIC X(6)    VALUE LOW-VALUE.                 
019000*                                                                         
019100   03  W-WDN5B1-HI-X.                                                     
019200     05  FILLER              PIC X(16)   VALUE HIGH-VALUE.                
019300*                                                                         
019400*- - - - - - - - - - NYCKLAR FÖR LÄSNING AV WLKATH01,21                   
019500   03  W-WDN501KY-X.                                                      
019600     05 W-IDCATNR-01         PIC 9(5)    VALUE ZERO.                      
019700     05 W-IDCATGRP-01        PIC 9(2)    VALUE ZERO.                      
019800     05 W-IDCATAVS-01        PIC 9(4)    VALUE ZERO.                      
019900*                                                                         
020000   03  W-WDN512KY-X.                                                      
020100     05 W-IDCATRAD           PIC 9(4)    VALUE ZERO.                      
020200     05 W-KDCATPUB           PIC X(6)    VALUE LOW-VALUE.                 
020300     SKIP3                                                                
020400*                        **** STATUS-KOD FRÅN IMS                         
020500   03  STATUS-WS             PIC XX.                                      
020600     88  SEGMENT-FINNS                   VALUE '  '.                      
020700     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
020800     88  BASEN-SLUT                      VALUE 'GB'.                      
020900     SKIP3                                                                
021000   03  GODK-STATUSKODER.                                                  
021100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021200     SKIP3                                                                
021300 01    SSA1                  PIC X(96).                                   
021400 01    SSA2                  PIC X(96).                                   
021500 01    SSA3                  PIC X(96).                                   
021600     EJECT                                                                
021700*                            IMS FUNKTIONSKODER                           
021800*01    -COPY W0003                                                        
021900     EJECT                                                                
022000*                            DLI INPUT-OUTPUT AREA                        
022100 01  DLI-IO-AREA.                                                         
022200   03  IO-AREA-1             PIC X(480)  VALUE SPACE.                     
022300     SKIP3                                                                
022400*  03  WLKATM01  -COPY WDN101  -PRE KAT-  -RED IO-AREA-1                  
022500     EJECT                                                                
022600*  03  WLBENA11  -COPY WDD311  -PRE BEN-  -RED IO-AREA-1                  
022700     EJECT                                                                
022800*  03  WLKATH21  -COPY WDN521  -PRE AVS-  -RED IO-AREA-1                  
022900     EJECT                                                                
023000   03  IO-AREA-2             PIC X(30)  VALUE SPACE.                      
023100*  03  WLKATJ01  -COPY WDN5B1  -PRE AVS-  -RED IO-AREA-2                  
023200     EJECT                                                                
023300 LINKAGE SECTION.                                                         
023400*01  -COPY W0009     -PRE MSG-                                            
023500     EJECT                                                                
023600*01  -COPY W0008     -PRE USEA-                                           
023700         05  FILLER           PIC X.                                      
023800*01  -COPY W0008     -PRE KAT-                                            
023900     05  FILLER              PIC X.                                       
024000*01  -COPY W0008     -PRE BEN-                                            
024100     05  FILLER              PIC X.                                       
024200*01  -COPY W0008     -PRE AVS-                                            
024300     05  FILLER              PIC X.                                       
024400*01  -COPY W0008     -PRE AVSB-                                           
024500     05  FILLER              PIC X.                                       
024600     EJECT                                                                
024700 PROCEDURE DIVISION USING MSG-PCB USEA-PCB KAT-PCB BEN-PCB                
024800                                      AVS-PCB AVSB-PCB.                   
024900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB KAT-PCB BEN-PCB               
025000                                      AVS-PCB AVSB-PCB.                   
025100     PERFORM IMS-GET-MSG                                                  
025200     IF SEGMENT-FINNS                                                     
025300       PERFORM A-INIT                                                     
025400       PERFORM B-KOLLA-NYCKLAR-O-INDATA                                   
025500       IF NYCKLAR-OK AND INDATA-OK                                        
025600         IF MFS-UPDATE                                                    
025700           CONTINUE                                                       
025800         ELSE                                                             
025900           IF MFS-FIRST                                                   
026000             PERFORM C-FOERSTA-SIDA                                       
026100           ELSE                                                           
026200             IF MFS-NEXT                                                  
026300               PERFORM D-NAESTA-SIDA                                      
026400             ELSE                                                         
026500               PERFORM E-SAMMA-SIDA                                       
026600             END-IF                                                       
026700           END-IF                                                         
026800           PERFORM F-LAES-VISA-INFO                                       
026900         END-IF                                                           
027000       END-IF                                                             
027100       COMPUTE MSG-KVLL = LENGTH OF MOD-w90421o1 + 4                      
027200       PERFORM IMS-INSERT-MSG                                             
027300     END-IF                                                               
027400                                                                          
027500     MOVE ZERO TO RETURN-CODE                                             
027600     GOBACK                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 A-INIT SECTION.                                                          
028000                                                                          
028100     IF MSG-DUBBLA-TRANSKODER                                             
028200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90421I1                 
028300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
028400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028500     ELSE                                                                 
028600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90421I1                  
028700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
028800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028900     END-IF                                                               
029000                                                                          
029100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
029200     MOVE MSG-IDPFK            TO MFS-IDPFK                               
029300     MOVE MFS-IDTRANS          TO W-IDTRANS                               
029400                                                                          
031100     MOVE LOW-VALUE  TO MSG-AREA                                          
031200     MOVE 'W90421O1' TO MFS-IDMOD                                         
031300     MOVE '9421'     TO MOD-IDTRANS                                       
031400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
031500                             MOD-TEMFSINF                                 
031600*                            MOD-IDARTNR-IN                               
031700*                            MOD-IDCATNR-IN                               
031900     IF EGEN-MID OR HELP-MID                                              
031910       CONTINUE                                                           
031920     ELSE                                                                 
032000       MOVE SPACE TO MFS-KDTRTYP                                          
032100       MOVE '7'   TO MFS-IDPFK                                            
032200     END-IF                                                               
032300                                                                          
033400     ACCEPT DAGENS-DATUM FROM DATE                                        
033500                                                                          
033600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
033700     MOVE DAGENS-DATUM                                                    
033800                   TO DAT-I-TIDATUM                                       
033900                                                                          
034000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
034100                     DAT-O-TIDATUM DAT-KDSVAR                             
034200                                                                          
034300     IF DAT-KDSVAR-OK                                                     
034400****             HÄMTA SEKELSIFFROR                                       
034500                                                                          
034600       MOVE DAT-TISEKEL    TO DAGENS-AAR(1:2)                             
034700                                                                          
034800     ELSE                                                                 
034900         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
035000         DELIMITED BY SIZE INTO FELTEXT                                   
035100         CALL FELLOG                                                      
035200     END-IF                                                               
035300                                                                          
035400     MOVE DAGENS-DATUM(1:2)  TO DAGENS-AAR(3:2)                           
035500                                                                          
035600     COMPUTE WS-TIAAAA(1) = DAGENS-AAR - 1                                
035700     COMPUTE WS-TIAAAA(2) = DAGENS-AAR                                    
035800     COMPUTE WS-TIAAAA(3) = DAGENS-AAR + 1                                
035900     COMPUTE WS-TIAAAA(4) = DAGENS-AAR + 2                                
036000     .                                                                    
036100     EJECT                                                                
036200 B-KOLLA-NYCKLAR-O-INDATA SECTION.                                        
036300     SKIP2                                                                
036310     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036320     MOVE '001'             TO MSGI-KDCALL                                
036321     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036330                               MSGI-IDLTERM-USER                          
036350     MOVE '9421'            TO MSGI-IDTRANS                               
036360     IF GODK-MID                                                          
036370         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
036380     END-IF                                                               
036390     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036391                                                                          
036392     IF MSGI-IDLAND-SPR = 'SE'                                            
036394       MOVE 'S  ' TO W-IDSKYLT                                            
036395                     MED-IDSKYLT                                          
036396       MOVE +1 TO SPRAK-IX                                                
036397     ELSE                                                                 
036398       MOVE 'GB ' TO W-IDSKYLT                                            
036399                     MED-IDSKYLT                                          
036400       MOVE +2 TO SPRAK-IX                                                
036404     END-IF                                                               
036405                                                                          
036410     MOVE JA TO  NYCKLAR-SW  INDATA-SW                                    
036500**********************                                                    
036600*    IF NOT EGEN-MID                                                      
036700*      MOVE '00000' TO MID-IDCATNR-IN                                     
036800*    END-IF                                                               
036900                                                                          
036910*    -- KONTROLL AV IDARTNR                                               
037000     IF MID-IDARTNR-IN NOT = ALL '+'                                      
037500       MOVE '7'            TO MFS-IDPFK                                   
037600       MOVE SPACE          TO MFS-KDTRTYP                                 
037700     END-IF                                                               
037710*                                                                         
037714     MOVE MSGI-IDARTNR    TO IDARTNR-WS                                   
037800     IF IDARTNR-WS NUMERIC AND IDARTNR-WS > ZERO                          
037900       MOVE IDARTNR-WS     TO W-IDARTNR                                   
038000     ELSE                                                                 
038100       MOVE NEJ TO NYCKLAR-SW                                             
038200     END-IF                                                               
038300                                                                          
038400*    IF MID-IDCATNR-IN = ALL '+'                                          
038500*      MOVE MID-IDCATNR-UT TO IDKATNR-WS                                  
038600*      INSPECT IDKATNR-WS REPLACING LEADING SPACE BY ZERO                 
038700*    ELSE                                                                 
038800*      MOVE MID-IDCATNR-IN TO IDKATNR-WS                                  
038900*      MOVE '7'             TO MFS-IDPFK                                  
039000*      MOVE SPACE           TO MFS-KDTRTYP                                
039100*    END-IF                                                               
039200                                                                          
039300* KOLLA INDATA, KATALOG (FRÅN)                                            
039400*    IF INDATA-OK                                                         
039500*      IF IDKATNR-WS NOT NUMERIC                                          
039600*        MOVE MFS-NUM-FIELD-WRONG TO MOD-IDCATNR-IN-ATTR                  
039700*        MOVE NEJ TO INDATA-SW                                            
039800*      ELSE                                                               
039900*        MOVE MFS-NUM-FIELD-OK TO MOD-IDCATNR-IN-ATTR                     
040000*      END-IF                                                             
040100*    END-IF                                                               
040200                                                                          
040300* FLYTTA PÅGÅENDE VISAD KATALOG                                           
040400     MOVE MID-IDCATNR TO CURR-IDKATNR-WS                                  
040500     INSPECT CURR-IDKATNR-WS REPLACING LEADING SPACE BY ZERO              
040600                                                                          
040700* FLYTTA SIDRÄKNARE                                                       
040800*    IF NOT EGEN-MID                                                      
040900*       MOVE 1 TO SIDRAEK-WS                                              
041000*    ELSE                                                                 
041100*       MOVE MID-SIDRAEK TO SIDRAEK-WS                                    
041200*    END-IF                                                               
041300                                                                          
041400* FLYTTA NYCKLAR TILL MOD                                                 
041500     IF NYCKLAR-OK                                                        
041600*      MOVE IDARTNR-WS      TO MOD-IDARTNR-UT                             
041700*      INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
041710       continue                                                           
041800     ELSE                                                                 
041900       MOVE FEL3(SPRAK-IX)  TO MOD-TEMFSFEL                               
042000*      MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
042100       PERFORM MFS-RENSA-FAELT-UT                                         
042200       MOVE +1              TO R-IX  K-IX                                 
042300       PERFORM MFS-RENSA-OSKRIVNA-FAELT-UTRAD                             
042400     END-IF                                                               
042500                                                                          
042600* FLYTTA INDATA TILL MOD                                                  
042700     IF INDATA-FEL                                                        
042800       MOVE MFS-ERASE-FIELD TO MOD-IDCATNR-UT                             
042900*                              MOD-IDCATNR-IN                             
043000       MOVE FEL1(SPRAK-IX)  TO MOD-TEMFSFEL                               
043100       PERFORM MFS-RENSA-FAELT-UT                                         
043200       MOVE +1              TO R-IX  K-IX                                 
043300       PERFORM MFS-RENSA-OSKRIVNA-FAELT-UTRAD                             
043400     ELSE                                                                 
043500       MOVE IDKATNR-WS      TO MOD-IDCATNR-UT                             
043600       INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE             
043700       IF MOD-IDCATNR-UT = SPACE                                          
043800         MOVE '    0' TO MOD-IDCATNR-UT                                   
043900       END-IF                                                             
043910       CONTINUE                                                           
044000     END-IF                                                               
044100     IF SIDRAEK-WS = 0, MOVE 1 TO SIDRAEK-WS, END-IF                      
044200*    MOVE SIDRAEK-WS           TO MOD-SIDRAEK                             
044300     .                                                                    
044400     EJECT                                                                
044500 C-FOERSTA-SIDA SECTION.                                                  
044600     SKIP2                                                                
044700*--- DETTA ÄR FÖRSTA SIDAN                                                
044800     MOVE MED2(SPRAK-IX) TO MOD-TEMFSFEL                                  
044900     MOVE JA             TO ALLT-SW                                       
045000     MOVE IDARTNR-WS     TO W-IDARTNR                                     
045100                                                                          
045200     PERFORM IMS-GU-BEN-BSEQ                                              
045300     IF SEGMENT-FINNS                                                     
045400       PERFORM IMS-GNP-BENA11                                             
045500*      IF SEGMENT-FINNS                                                   
045600*        MOVE BEN-TEXT-BEART TO MOD-BEART                                 
045700*      ELSE                                                               
045800*        MOVE BEN-SAKNAS(SPRAK-IX) TO MOD-BEART                           
045900*      END-IF                                                             
046000     ELSE                                                                 
046100*      MOVE ART-SAKNAS(SPRAK-IX) TO MOD-BEART                             
046110       CONTINUE                                                           
046200     END-IF                                                               
046300     IF SIDRAEK-WS < 2                                                    
046400       IF IDKATNR-WS > ZERO                                               
046500         MOVE IDKATNR-WS TO W-IDCATNR-L                                   
046600       ELSE                                                               
046700         MOVE LOW-VALUE  TO W-IDCATNR-XL                                  
046800       END-IF                                                             
046900       MOVE HIGH-VALUE TO W-IDCATNR-XH                                    
047000     ELSE                                                                 
047100       MOVE CURR-IDKATNR-WS TO W-IDCATNR-L                                
047200                               W-IDCATNR-H                                
047300       MOVE 1            TO SIDRAEK-WS                                    
047400     END-IF                                                               
047500     MOVE LOW-VALUE      TO W-WDN5B1-LO-X                                 
047600     .                                                                    
047700     EJECT                                                                
047800 D-NAESTA-SIDA SECTION.                                                   
047900     SKIP2                                                                
048000     MOVE JA TO ALLT-SW                                                   
048100     IF MID-IDCATNR-NEXT = ZERO                                           
048200       IF MID-IDCATGRP-NEXT > ZERO                                        
048300*------- VIDARELÄSNING INOM SAMMA KATALOG                                 
048400         MOVE CURR-IDKATNR-WS  TO W-IDCATNR-L                             
048500                                  W-IDCATNR-H                             
048600         ADD  1 TO SIDRAEK-WS                                             
048700       ELSE                                                               
048800*------- SLUT PÅ KATALOGER, LÄSNING FRÅN BÖRJAN IGEN                      
048900         IF IDKATNR-WS > ZERO                                             
049000           MOVE IDKATNR-WS TO W-IDCATNR-L                                 
049100         ELSE                                                             
049200           MOVE LOW-VALUE  TO W-IDCATNR-XL                                
049300         END-IF                                                           
049400         MOVE HIGH-VALUE TO W-IDCATNR-XH                                  
049500*------- DETTA ÄR FÖRSTA SIDAN                                            
049600         MOVE MED2(SPRAK-IX) TO MOD-TEMFSFEL                              
049700         MOVE 1 TO SIDRAEK-WS                                             
049800       END-IF                                                             
049900     ELSE                                                                 
050000*----- VIDARELÄSNING PÅ NÄSTA KATALOG                                     
050100       MOVE MID-IDCATNR-NEXT TO W-IDCATNR-L                               
050200       MOVE HIGH-VALUE       TO W-IDCATNR-XH                              
050300                                                                          
050400       MOVE 1 TO SIDRAEK-WS                                               
050500     END-IF                                                               
050600                                                                          
050700     MOVE MID-IDCATGRP-NEXT TO  W-IDCATGRP-LO                             
050800     MOVE MID-IDCATAVS-NEXT TO  W-IDCATAVS-LO                             
050900     MOVE MID-IDCATRAD-NEXT TO  W-IDCATRAD-LO                             
051000     MOVE MID-KDCATPUB-R-NEXT TO WS-KDCATPUB-R-AVV                        
051100     PERFORM S50-Y2K-KDCATPUB-R                                           
051200     MOVE WS-KDCATPUB-AAAAVV                                              
051300                            TO  W-KDCATPUB-LO                             
051400     IF MID-KDCATPUB-R-NEXT = SPACE                                       
051500        MOVE LOW-VALUE TO W-KDCATPUB-LO                                   
051600     END-IF                                                               
051700                                                                          
051800*    MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
051900     .                                                                    
052000     EJECT                                                                
052100 E-SAMMA-SIDA SECTION.                                                    
052200     SKIP2                                                                
052300     MOVE JA TO ALLT-SW                                                   
052400     MOVE CURR-IDKATNR-WS TO   W-IDCATNR-L                                
052500     MOVE HIGH-VALUE        TO W-IDCATNR-XH                               
052600     MOVE MID-IDCATGRP-SPAR TO W-IDCATGRP-LO                              
052700     MOVE MID-IDCATAVS-SPAR TO W-IDCATAVS-LO                              
052800     MOVE MID-IDCATRAD-SPAR TO W-IDCATRAD-LO                              
052900     MOVE MID-KDCATPUB-R-SPAR TO WS-KDCATPUB-R-AVV                        
053000     PERFORM S50-Y2K-KDCATPUB-R                                           
053100     MOVE WS-KDCATPUB-AAAAVV                                              
053200                            TO W-KDCATPUB-LO                              
053300     IF MID-KDCATPUB-R-spar = SPACE                                       
053400       MOVE LOW-VALUE TO W-KDCATPUB-LO                                    
053500     END-IF                                                               
053600                                                                          
053700*    MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
053800     .                                                                    
053900     EJECT                                                                
054000 F-LAES-VISA-INFO SECTION.                                                
054100                                                                          
054200     PERFORM FA-HITTA-RAETT-FORDON                                        
054300                                                                          
054400     IF SEGMENT-FINNS                                                     
054500       MOVE KAT-KAT-BEMASTER TO MOD-BEMASTER                              
054600       MOVE W-IDCATNR-L TO W-IDCATNR-H                                    
054700                           W-IDCATNR-01                                   
054800       PERFORM IMS-GU-AVS-BSEQ                                            
054900*      MOVE AVS-AVSB-IDCATNR  TO MOD-IDCATNR                              
055000       MOVE AVS-AVSB-IDCATGRP TO MOD-IDCATGRP-SPAR                        
055100       MOVE AVS-AVSB-IDCATAVS TO MOD-IDCATAVS-SPAR                        
055200       MOVE AVS-AVSB-IDCATRAD TO MOD-IDCATRAD-SPAR                        
055300       MOVE AVS-AVSB-KDCATPUB-FOM (4:3)                                   
055400                              TO MOD-KDCATPUB-R-SPAR                      
055500       IF MOD-KDCATPUB-R-SPAR = LOW-VALUE                                 
055600          MOVE SPACE TO MOD-KDCATPUB-R-SPAR                               
055700       END-IF                                                             
055800                                                                          
055900       MOVE +1 TO R-IX K-IX                                               
056000       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
056100                        (R-IX = RAD-MAX AND K-IX > KOL-MAX)               
056200         MOVE AVS-AVSB-IDCATGRP TO W-IDCATGRP-01                          
056300         MOVE AVS-AVSB-IDCATAVS TO W-IDCATAVS-01                          
056400         MOVE AVS-AVSB-IDCATRAD TO W-IDCATRAD                             
056500         MOVE AVS-AVSB-KDCATPUB-FOM                                       
056600                                TO W-KDCATPUB                             
056700                                                                          
056800         PERFORM IMS-GU-WLKATH21                                          
056900         IF SEGMENT-FINNS                                                 
057000*          MOVE W-IDCATGRP-01    TO MOD-IDCATGRP(R-IX, K-IX)              
057100*          MOVE W-IDCATAVS-01    TO MOD-IDCATAVS(R-IX, K-IX)              
057200*          MOVE W-IDCATRAD       TO MOD-IDCATRAD(R-IX, K-IX)              
057300*          MOVE W-KDCATPUB (4:3) TO MOD-KDCATPUB-R(R-IX, K-IX)            
057400*          MOVE AVS-ART-IDCATPOS TO MOD-IDCATPOS(R-IX, K-IX)              
057500*          INSPECT MOD-IDCATPOS(R-IX, K-IX)                               
057600*                          REPLACING LEADING ZERO BY SPACE                
057700           MOVE +1      TO KVANT-IX                                       
057800           MOVE '-  ' TO WS-KVKOL                                         
057900           PERFORM UNTIL ((KVANT-IX > KVANT-MAX)                          
058000                    OR (WS-KVKOL NOT = '-  '))                            
058100             IF AVS-ART-KVKOL(KVANT-IX) NOT = SPACE                       
058200               IF AVS-ART-KVKOL(KVANT-IX) (1:1) NOT = SPACE               
058300                 MOVE AVS-ART-KVKOL(KVANT-IX) TO WS-KVKOL                 
058400               ELSE                                                       
058500                 IF AVS-ART-KVKOL(KVANT-IX) (2:1) NOT = SPACE             
058600                   MOVE AVS-ART-KVKOL(KVANT-IX) (2:2) TO WS-KVKOL         
058700                 ELSE                                                     
058800                   IF AVS-ART-KVKOL(KVANT-IX) (3:1) NOT = SPACE           
058900                     MOVE AVS-ART-KVKOL(KVANT-IX) (3:1)                   
059000                                                      TO WS-KVKOL         
059100                   END-IF                                                 
059200                 END-IF                                                   
059300               END-IF                                                     
059400             ELSE                                                         
059500               IF AVS-ART-KDFBX = 'F'                                     
059600                 MOVE SPACE TO WS-KVKOL                                   
059700               END-IF                                                     
059800             END-IF                                                       
059900             ADD +1 TO KVANT-IX                                           
060000           END-PERFORM                                                    
060100*          MOVE WS-KVKOL TO MOD-KVKOL(R-IX, K-IX)                         
060200           IF R-IX = RAD-MAX                                              
060300           AND K-IX = KOL-MAX                                             
060400             ADD +1 TO K-IX                                               
060500           ELSE                                                           
060600             IF R-IX < RAD-MAX                                            
060700               ADD +1 TO R-IX                                             
060800             ELSE                                                         
060900               MOVE +1 TO R-IX                                            
061000               ADD +1 TO K-IX                                             
061100             END-IF                                                       
061200           END-IF                                                         
061300           PERFORM IMS-GN-AVS-BSEQ                                        
061400         ELSE                                                             
061500           MOVE +1 TO R-IX K-IX                                           
061600           PERFORM MFS-RENSA-OSKRIVNA-FAELT-UTRAD                         
061700           MOVE 'ALLVARLIGT FEL - KONTAKTA SYSTEMAVD.'                    
061800                                         TO MOD-TEMFSFEL                  
061900         END-IF                                                           
062000       END-PERFORM                                                        
062100*----- VISNING SLUT                                                       
062200                                                                          
062300       IF SEGMENT-FINNS                                                   
062400         MOVE ZERO              TO MOD-IDCATNR-NEXT                       
062500         MOVE AVS-AVSB-IDCATGRP TO MOD-IDCATGRP-NEXT                      
062600         MOVE AVS-AVSB-IDCATAVS TO MOD-IDCATAVS-NEXT                      
062700         MOVE AVS-AVSB-IDCATRAD TO MOD-IDCATRAD-NEXT                      
062800         MOVE AVS-AVSB-KDCATPUB-FOM (4:3)                                 
062900                                TO MOD-KDCATPUB-R-NEXT                    
063000         IF MOD-KDCATPUB-R-NEXT = LOW-VALUE                               
063100            MOVE SPACE TO MOD-KDCATPUB-R-NEXT                             
063200         END-IF                                                           
063300         MOVE MED1(SPRAK-IX)    TO MOD-TEMFSINF                           
063400       ELSE                                                               
063500         IF SEGMENT-SAKNAS                                                
063600           PERFORM MFS-RENSA-OSKRIVNA-FAELT-UTRAD                         
063700           MOVE HIGH-VALUE TO W-IDCATNR-XH                                
063800           MOVE LOW-VALUE  TO W-WDN5B1-LO-X                               
063900           ADD +1          TO W-IDCATNR-L                                 
064000                                                                          
064100           PERFORM FA-HITTA-RAETT-FORDON                                  
064200                                                                          
064300           IF SEGMENT-FINNS                                               
064400             PERFORM IMS-GU-AVS-BSEQ                                      
064500*---------   FLER KATALOGER FINNS                                         
064600                                                                          
064700             MOVE AVS-AVSB-IDCATNR  TO MOD-IDCATNR-NEXT                   
064800             MOVE AVS-AVSB-IDCATGRP TO MOD-IDCATGRP-NEXT                  
064900             MOVE AVS-AVSB-IDCATAVS TO MOD-IDCATAVS-NEXT                  
065000             MOVE AVS-AVSB-IDCATRAD TO MOD-IDCATRAD-NEXT                  
065100             MOVE AVS-AVSB-KDCATPUB-FOM (4:3)                             
065200                                    TO MOD-KDCATPUB-R-NEXT                
065300             IF MOD-KDCATPUB-R-NEXT = LOW-VALUE                           
065400                MOVE SPACE TO MOD-KDCATPUB-R-NEXT                         
065500             END-IF                                                       
065600             MOVE MED3(SPRAK-IX)    TO MOD-TEMFSINF                       
065700           ELSE                                                           
065800*---------   INGA FLER KATALOGER MED RÄTT URVAL, SISTA SIDAN              
065900                                                                          
066000             PERFORM MFS-RENSA-OSKRIVNA-FAELT-UTRAD                       
066100             MOVE ZEROES TO MOD-IDCATNR-NEXT                              
066200                            MOD-IDCATGRP-NEXT                             
066300                            MOD-IDCATAVS-NEXT                             
066400                            MOD-IDCATRAD-NEXT                             
066500                            MOD-KDCATPUB-R-NEXT                           
066600             MOVE MED4(SPRAK-IX) TO MOD-TEMFSFEL                          
066700           END-IF                                                         
066800         ELSE                                                             
066900*--------- BASEN SLUT, SISTA SIDAN                                        
067000           PERFORM MFS-RENSA-OSKRIVNA-FAELT-UTRAD                         
067100           MOVE ZEROES TO MOD-IDCATNR-NEXT                                
067200                          MOD-IDCATGRP-NEXT                               
067300                          MOD-IDCATAVS-NEXT                               
067400                          MOD-IDCATRAD-NEXT                               
067500                          MOD-KDCATPUB-R-NEXT                             
067600           MOVE MED4(SPRAK-IX)  TO MOD-TEMFSFEL                           
067700         END-IF                                                           
067800       END-IF                                                             
067900     ELSE                                                                 
068000*      INGEN TRÄFF PÅ ANGIVNA SÖKVÄRDEN                                   
068100       MOVE FEL2(SPRAK-IX) TO MOD-TEMFSFEL                                
068200       PERFORM MFS-RENSA-FAELT-UT                                         
068300       MOVE +1 TO R-IX                                                    
068400                  K-IX                                                    
068500       PERFORM MFS-RENSA-OSKRIVNA-FAELT-UTRAD                             
068600     END-IF                                                               
068700*    MOVE SIDRAEK-WS TO MOD-SIDRAEK                                       
068800     .                                                                    
068900     EJECT                                                                
069000 FA-HITTA-RAETT-FORDON SECTION.                                           
069100                                                                          
069200     PERFORM IMS-GU-AVS-BSEQ                                              
069300     IF SEGMENT-FINNS                                                     
069400       MOVE AVS-AVSB-IDCATNR TO W-IDCATNR-L                               
069500       PERFORM IMS-GU-WLKATM01                                            
069600       IF SEGMENT-FINNS                                                   
069700         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                       
069800           OR KAT-KAT-KDFORDON = 'PV' OR 'NL' OR 'RE'                     
069900           ADD +1 TO W-IDCATNR-L                                          
070000           PERFORM IMS-GN-AVS-BSEQ                                        
070100           IF SEGMENT-FINNS                                               
070200             MOVE AVS-AVSB-IDCATNR TO W-IDCATNR-L                         
070300             PERFORM IMS-GU-WLKATM01                                      
070400           END-IF                                                         
070500         END-PERFORM                                                      
070600       END-IF                                                             
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 MFS-RENSA-FAELT-UT SECTION.                                              
071100     SKIP2                                                                
071200     MOVE MFS-RENSA-FAELT TO                                              
071210*                            MOD-IDCATNR                                  
071300                             MOD-BEMASTER                                 
071400                             MOD-IDCATGRP-SPAR                            
071500                             MOD-IDCATAVS-SPAR                            
071600                             MOD-IDCATRAD-SPAR                            
071700                             MOD-KDCATPUB-R-SPAR                          
071800     MOVE ZEROES          TO MOD-IDCATGRP-NEXT                            
071900                             MOD-IDCATAVS-NEXT                            
072000                             MOD-IDCATRAD-NEXT                            
072100                             MOD-KDCATPUB-R-NEXT                          
072200                             MOD-IDCATNR-NEXT                             
072300     MOVE  1              TO SIDRAEK-WS                                   
072400     .                                                                    
072500     SKIP2                                                                
072600 MFS-RENSA-OSKRIVNA-FAELT-UTRAD SECTION.                                  
072700     SKIP2                                                                
072800     PERFORM UNTIL K-IX > KOL-MAX                                         
072900        PERFORM UNTIL R-IX > RAD-MAX                                      
073000           MOVE MFS-RENSA-FAELT TO MOD-KOL(R-IX, K-IX)                    
073100           ADD +1 TO R-IX                                                 
073200        END-PERFORM                                                       
073300        MOVE +1 TO R-IX                                                   
073400        ADD  +1 TO K-IX                                                   
073500     END-PERFORM                                                          
073600     .                                                                    
073700     EJECT                                                                
073800*                                                                         
073900* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
074000* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
074100*                                                                         
074200*    -COPY W150Y2K1                                                       
074300     EJECT                                                                
074400* IMS SEKTIONER                                                           
074500     SKIP3                                                                
074600 IMS-GET-MSG SECTION.                                                     
074700                                                                          
074800     MOVE '  QC' TO GODK-STATUSKODER                                      
074900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075100     PERFORM IMS-STATUSKONTROLL                                           
075200     .                                                                    
075300     SKIP3                                                                
075400 IMS-INSERT-MSG SECTION.                                                  
075500                                                                          
075600*    IF MSGI-IDLAND-SPR = 'SE'                                            
075700*      MOVE '0' TO MFS-KDHUVOMR                                           
075800*    END-IF                                                               
075900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
076000     MOVE SPACE TO GODK-STATUSKODER                                       
076100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076300     PERFORM IMS-STATUSKONTROLL                                           
076400     .                                                                    
076500     EJECT                                                                
076600 IMS-GU-WLKATM01 SECTION.                                                 
076700                                                                          
076800     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-XL ')'                        
076900          DELIMITED BY SIZE INTO SSA1                                     
077000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
077100     CALL CBLTDLI USING GU KAT-PCB IO-AREA-1 SSA1                         
077200     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500     EJECT                                                                
077600 IMS-GU-BEN-BSEQ SECTION.                                                 
077700     SKIP2                                                                
077800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
077900          DELIMITED BY SIZE INTO SSA1                                     
078000     MOVE '  GE' TO GODK-STATUSKODER                                      
078100     CALL CBLTDLI USING GU BEN-PCB IO-AREA-1 SSA1                         
078200     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
078300     PERFORM IMS-STATUSKONTROLL                                           
078400     .                                                                    
078500     SKIP3                                                                
078600 IMS-GNP-BENA11   SECTION.                                                
078700     SKIP2                                                                
078800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
078900          DELIMITED BY SIZE INTO SSA1                                     
079000     MOVE '  GE' TO GODK-STATUSKODER                                      
079100     CALL CBLTDLI USING GNP BEN-PCB IO-AREA-1 SSA1                        
079200     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
079300     PERFORM IMS-STATUSKONTROLL                                           
079400     .                                                                    
079500     EJECT                                                                
079600 IMS-GU-AVS-BSEQ   SECTION.                                               
079700     SKIP2                                                                
079800     STRING 'WLKATJ01(WDN5B1KY>=' W-IDARTNR-X W-IDCATNR-XL                
079900                                  W-WDN5B1-LO-X                           
080000                    '&WDN5B1KY <' W-IDARTNR-X W-IDCATNR-XH                
080100                                  W-WDN5B1-HI-X ')'                       
080200          DELIMITED BY SIZE INTO SSA1                                     
080300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
080400     CALL CBLTDLI USING GU AVSB-PCB IO-AREA-2 SSA1                        
080500     MOVE AVSB-STATUS-CODE TO STATUS-WS                                   
080600     PERFORM IMS-STATUSKONTROLL                                           
080700     .                                                                    
080800     SKIP2                                                                
080900 IMS-GN-AVS-BSEQ   SECTION.                                               
081000                                                                          
081100     STRING 'WLKATJ01(WDN5B1KY>=' W-IDARTNR-X W-IDCATNR-XL                
081200                                  W-WDN5B1-LO-X                           
081300                    '&WDN5B1KY<=' W-IDARTNR-X W-IDCATNR-XH                
081400                                  W-WDN5B1-HI-X ')'                       
081500          DELIMITED BY SIZE INTO SSA1                                     
081600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
081700     CALL CBLTDLI USING GN AVSB-PCB IO-AREA-2 SSA1                        
081800     MOVE AVSB-STATUS-CODE TO STATUS-WS                                   
081900     PERFORM IMS-STATUSKONTROLL                                           
082000     .                                                                    
082100     EJECT                                                                
082200 IMS-GU-WLKATH21     SECTION.                                             
082300                                                                          
082400     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
082500          DELIMITED BY SIZE INTO SSA1                                     
082600     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
082700          DELIMITED BY SIZE INTO SSA2                                     
082800     MOVE 'WLKATH21 '         TO SSA3                                     
082900     MOVE '  GE' TO GODK-STATUSKODER                                      
083000     CALL CBLTDLI USING GU AVS-PCB IO-AREA-1 SSA1 SSA2 SSA3               
083100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
083200     PERFORM IMS-STATUSKONTROLL                                           
083300     .                                                                    
083400     SKIP2                                                                
083500 IMS-STATUSKONTROLL SECTION.                                              
083600                                                                          
083700     SET STATUS-IX TO 1                                                   
083800     SEARCH GODK-STATUS                                                   
083900       AT END CALL FELLOG                                                 
084000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
084100     END-SEARCH                                                           
084200     .                                                                    
