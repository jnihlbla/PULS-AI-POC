001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5022200.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/09/16.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET HAR TVÅ FUNKTIONER, NÄMLIGEN:                         
002100*        -HANTERA NYREGISTRERING AV KONTO, KOSTNADSCENTER SAMT            
002200*        ANALYSNUMMER                                                     
002300*        -HANTERA UPPDATERING AV KONTO, KOSTNADSCENTER SAMT               
002400*        ANALYSNUMMMER                                                    
002500*        DETTA SKER M.H.A INMATAD DATA FRÅN BILD 5222                     
002600*                                                                         
002710*        PROGRAMMET UPPDATERAR WL5121 (WDGX)                              
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W5T222                                              
003100*        MID:         W5I22201                                            
003200*                                                                         
003300*    UTDATA.                                                              
003400*        MOD:         W5O22201                                            
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 WORKING-STORAGE SECTION.                                                 
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W5022200'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200                                                                          
005210 77  BYT-SW                      PIC X       VALUE 'N'.                   
005220     88  BYT-BILD                            VALUE 'J'.                   
005230     88  BYT-EJ-BILD                         VALUE 'N'.                   
005240                                                                          
005300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005301     88  ALLT-OK                             VALUE 'J'.                   
005302                                                                          
005303 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005304     88  INDATA-OK                           VALUE 'J'.                   
005310     88  INDATA-FEL                          VALUE 'N'.                   
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '5222'.                
006100     88  GODK-MID                            VALUE '5221' '5222'          
006200                                                   '5223' '5224'          
006300                                                   '5225' '5226'          
006400                                                   '5227' '5228'          
006500                                                   '5229'.                
006600     88  HELP-MID                            VALUE '0551'.                
006700     EJECT                                                                
006800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006900 01  GENERELLA-SUBPROGRAM.                                                
007000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007800     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
008001     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008002     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008003     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008010     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008800     SKIP3                                                                
008900*01 -COPY WMSGINIT                                                        
009100     EJECT                                                                
009110*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009120 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
009130*                                                                         
009140 01  SPAR-AREA.                                                           
009150     03  SPAR-IDTRANS             PIC X(4)    VALUE '5222'.               
009151     03  SPAR-BILD                PIC X(4)    VALUE SPACE.                
009160     EJECT                                                                
009200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009300*                                                                         
009400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009500     SKIP3                                                                
009600*01  MID -COPY W5I22201                                                   
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009900     SKIP3                                                                
010000*01  -COPY WMSGAREA                                                       
010100     EJECT                                                                
010200     03  MOD REDEFINES MSG-AREA.                                          
010300*      05  -COPY W5O22201                                                 
010400     EJECT                                                                
010410 01  W-PROG-TO-PROG-SW-5223.                                              
010420     03  M-SW-LL-5223            PIC S9(4)   VALUE +240 COMP SYNC.        
010430     03  M-SW-Z1-Z2-5223         PIC X(2)    VALUE LOW-VALUE.             
010440     03  M-SW-KDTRANS-5223       PIC X(8)    VALUE 'W5T223  '.            
010450     03  M-SW-IDTRANS-5223       PIC X(4)    VALUE '5222'.                
010460     03  M-SW-KDMFSTYP-5223      PIC X(1)    VALUE '2'.                   
010470                                                                          
010480*    03  MID -COPY W5I22301 -PRE 5223-                                    
010490     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010600     SKIP3                                                                
010700*01  -COPY WMFSAREA                                                       
010800     EJECT                                                                
010900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011000*                                                                         
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 01  NYCKLAR-TILL-DLI.                                                    
011500     03  W-WDGXKEY-X.                                                     
011501         05  W-IDHTYP            PIC X(4)    VALUE '5121'.                
011502         05  W-IDFTG             PIC X(2)    VALUE SPACE.                 
011502         05  W-WDGXKEY           PIC X(24)   VALUE LOW-VALUE.             
011503     03  W-KEY5122-X.                                                     
011510         05  W-IDKONTO           PIC S9(11)  COMP-3 VALUE ZERO.           
011520         05  W-IDPRCTR           PIC X(10)   VALUE SPACE.                 
011600     SKIP2                                                                
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700 01  SSA2                        PIC X(64).                               
012800     EJECT                                                                
012900*    --- IMS FUNKTIONSKODER                                               
013000*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400                                                                          
013501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
013502 01  DLI-IO-WDGX5121.                                                     
013510*    03  -COPY WDGX5121 -PRE WDGX5121-                                    
013520                                                                          
013530 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
013540 01  DLI-IO-WDGX5122.                                                     
013550*    03  -COPY WDGX5122 -PRE WDGX5122-                                    
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000*01  -COPY W0009   -PRE MSG-                                              
014010     EJECT                                                                
014020*01  -COPY W0009   -PRE ALT-                                              
014030     EJECT                                                                
014100*01  -COPY W0008   -PRE USEA-                                             
014200     05  FILLER                  PIC X.                                   
014301                                                                          
014302*01  -COPY W0008  -PRE 5121-                                              
014310     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014501 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 5121-PCB.             
014502 MAIN SECTION.                                                            
014510     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 5121-PCB.             
014600                                                                          
014800     PERFORM IMS-GET-MSG                                                  
014900     IF SEGMENT-FINNS                                                     
015000       PERFORM A-INIT                                                     
015100       PERFORM B-KOLLA-NYCKLAR                                            
015200       IF NYCKLAR-OK                                                      
015303         IF MFS-UPDATE                                                    
015304           PERFORM G-KOLLA-INPUT                                          
015305           IF INDATA-OK                                                   
015306             PERFORM H-UPPDATERA                                          
015307           END-IF                                                         
015310         ELSE                                                             
015501           IF MFS-FIRST                                                   
015502             PERFORM C-FOERSTA-SIDA                                       
015503           ELSE                                                           
015504             PERFORM E-SAMMA-SIDA                                         
015510           END-IF                                                         
015610         END-IF                                                           
015620         IF ALLT-OK                                                       
015700           PERFORM F-LAES-VISA-INFO                                       
015800         END-IF                                                           
015900       END-IF                                                             
016000       IF BYT-EJ-BILD                                                     
016100         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22201 + 4                    
016200         PERFORM IMS-INSERT-MSG                                           
016300       END-IF                                                             
016400     END-IF                                                               
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     IF MSG-DUBBLA-TRANSKODER                                             
017300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I22201                 
017400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I22201                  
017800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018000     END-IF                                                               
018100                                                                          
018200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018500                                                                          
018600     MOVE LOW-VALUE TO MSG-AREA                                           
018700     MOVE 'W5O222N1' TO MFS-IDMOD                                         
018800     MOVE '5222' TO MOD-IDTRANS                                           
018900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019000                                                                          
019100     IF EGEN-MID OR HELP-MID                                              
019200       CONTINUE                                                           
019300     ELSE                                                                 
019400       MOVE SPACE TO MFS-KDTRTYP                                          
019500       MOVE '7' TO MFS-IDPFK                                              
019600     END-IF                                                               
019900     .                                                                    
020000     EJECT                                                                
020100 B-KOLLA-NYCKLAR SECTION.                                                 
020200                                                                          
020300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020400     MOVE '001'             TO MSGI-KDCALL                                
020500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020700     MOVE '5222'            TO MSGI-IDTRANS                               
020701     INSPECT MID-IDKONTO-IN REPLACING LEADING SPACE BY ZERO               
020710     IF EGEN-MID OR GODK-MID                                              
020720       MOVE MID-IDKONTO-IN  TO MSGI-IDKONTO                               
020721       MOVE MID-IDPRCTR-IN  TO MSGI-IDPRCTR                               
020770     END-IF                                                               
020780     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020790     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020800                                                                          
020900     IF MSGI-IDLAND-SPR = 'GB'                                            
021000       MOVE 'GB' TO MED-IDSKYLT                                           
021100     ELSE                                                                 
021110       MOVE 'S' TO MED-IDSKYLT                                            
021120     END-IF                                                               
021200                                                                          
021300     MOVE JA TO NYCKLAR-SW                                                
021310     MOVE SPACE TO MOD-TEMFSFEL                                           
021400                                                                          
021500     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021501       MOVE MSGI-IDFTG      TO MOD-IDFTG-UT                               
                                     W-IDFTG                                    
021502     END-IF                                                               
021503                                                                          
021504*    -- KONTROLL AV IDKONTO                                               
021505     MOVE MFS-RENSA-FAELT TO MOD-IDKONTO-IN                               
021506                                                                          
021507     IF MSGI-IDKONTO NUMERIC AND MSGI-IDKONTO > ZERO                      
021508       MOVE MSGI-IDKONTO    TO W-IDKONTO                                  
021509     ELSE                                                                 
021510       MOVE NEJ             TO NYCKLAR-SW                                 
021520     END-IF                                                               
021601                                                                          
021602*    -- KONTROLL AV IDPRCTR                                               
021603     MOVE MFS-RENSA-FAELT   TO MOD-IDPRCTR-IN                             
021604                                                                          
021606     MOVE MSGI-IDPRCTR      TO W-IDPRCTR                                  
021610                                                                          
021611     IF GODK-MID OR NYCKLAR-OK                                            
021613       MOVE MSGI-IDKONTO    TO MOD-IDKONTO-UT                             
021614       INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE             
021615       MOVE MSGI-IDPRCTR    TO MOD-IDPRCTR-UT                             
021616     ELSE                                                                 
021617       MOVE MFS-RENSA-FAELT TO MOD-IDKONTO-UT                             
021618                               MOD-IDPRCTR-UT                             
021620     END-IF                                                               
021700                                                                          
021701     IF NYCKLAR-FEL                                                       
021710*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021720*---GODKÄND BILD                                                          
021730       IF GODK-MID                                                        
021900         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
022000         CALL WMEDKONV USING MED-WMEDAREA                                 
022100         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
022200         PERFORM MFS-RENSA-FAELT-IN                                       
022300         PERFORM MFS-RENSA-FAELT-UT                                       
022400       END-IF                                                             
022410     END-IF                                                               
022500     .                                                                    
022700     EJECT                                                                
022801 C-FOERSTA-SIDA SECTION.                                                  
022802                                                                          
022803     PERFORM MFS-RENSA-FAELT-IN                                           
022804     .                                                                    
022805     EJECT                                                                
022806 E-SAMMA-SIDA SECTION.                                                    
022807                                                                          
022809*---OM MID-IDANALYS ÄNDRATS OCH 'ENTER' AKTIVERATS                        
022810*---MÅSTE FÖLJANDE IF-SATS ANVÄNDAS FÖR ATT FÅ UT NÅGON DATA              
022811     IF MID-IDANALYS NOT = ALL '+'                                        
022814       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
022815       CALL WMEDKONV USING MED-WMEDAREA                                   
022816       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022817       PERFORM MFS-ROER-EJ-FAELT-IN                                       
022818       PERFORM MFS-LAES-IN-IGEN                                           
022819       MOVE NEJ TO ALLT-SW                                                
022820       MOVE '5222' TO SPAR-BILD                                           
022821     ELSE                                                                 
022822       IF SPAR-BILD = '5223'                                              
022823        AND MID-IDKONTO-IN  = ALL '+'                                     
022824        AND MID-IDPRCTR-IN  = ALL '+'                                     
022828         PERFORM I-BYT-BILD                                               
022829       ELSE                                                               
022830         MOVE '5222' TO SPAR-BILD                                         
022831         PERFORM MFS-RENSA-FAELT-IN                                       
022832       END-IF                                                             
022833     END-IF                                                               
022837     .                                                                    
022840     EJECT                                                                
022900 F-LAES-VISA-INFO SECTION.                                                
023000                                                                          
023100     PERFORM IMS-GU-WDGX5121                                              
023110     PERFORM IMS-GHU-WDGX5122                                             
023200                                                                          
023300     IF SEGMENT-SAKNAS                                                    
023301       IF MOD-TEMFSFEL = SPACE                                            
023302         MOVE 'ACCOUNT/PROFIT CENTER MISSING' TO MOD-TEMFSFEL             
023303       END-IF                                                             
023700       PERFORM MFS-RENSA-FAELT-UT                                         
023800     ELSE                                                                 
023900       MOVE WDGX5122-5122-IDANALYS TO MOD-IDANALYS                        
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
025102 G-KOLLA-INPUT SECTION.                                                   
025104     MOVE JA  TO INDATA-SW                                                
025105     IF MID-IDANALYS = ALL '+'                                            
025106       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
025107       CALL WMEDKONV USING MED-WMEDAREA                                   
025108       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025109       PERFORM MFS-ROER-EJ-FAELT-IN                                       
025110       PERFORM MFS-ROER-EJ-FAELT-UT                                       
025114       MOVE NEJ TO INDATA-SW                                              
025173     END-IF                                                               
025174     .                                                                    
025175     EJECT                                                                
025176 H-UPPDATERA SECTION.                                                     
025177                                                                          
025178     PERFORM IMS-GU-WDGX5121                                              
025179     PERFORM IMS-GHU-WDGX5122                                             
025180                                                                          
025181     MOVE MID-IDANALYS TO WDGX5122-5122-IDANALYS                          
025182     IF SEGMENT-FINNS                                                     
025183       PERFORM IMS-REPL-WDGX5122                                          
025184     ELSE                                                                 
025185       MOVE MSGI-IDKONTO TO WDGX5122-5122-IDKONTO                         
025186       MOVE MSGI-IDPRCTR TO WDGX5122-5122-IDPRCTR                         
025187       PERFORM IMS-ISRT-WDGX5122                                          
025188     END-IF                                                               
025190     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
025191     CALL WMEDKONV USING MED-WMEDAREA                                     
025192     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
025193     PERFORM MFS-FORM-ATTR                                                
025194     PERFORM MFS-RENSA-FAELT-IN                                           
025196     .                                                                    
025200     EJECT                                                                
025210 I-BYT-BILD SECTION.                                                      
025220     SKIP2                                                                
025230     MOVE NEJ TO ALLT-SW                                                  
025240     MOVE JA  TO BYT-SW                                                   
025250                                                                          
025297* ---SKICKAR VÄRDE TILL 5223-MID FÖR ATT SEDAN                            
025298* ---STARTA UPP 5223-BILDEN                                               
025299     MOVE LOW-VALUE          TO 5223-MID-W5I22301                         
025300     MOVE MSGI-IDKONTO       TO 5223-MID-IDKONTO-IN                       
025302     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22201 + 17                       
025303     PERFORM IMS-INSERT-ALT-MSG-5223                                      
025304     .                                                                    
025305     EJECT                                                                
025310 MFS-RENSA-FAELT-UT SECTION.                                              
025400                                                                          
025500*    --- ALLA UTDATA-FÄLT                                                 
025700     MOVE MFS-RENSA-FAELT TO MOD-IDANALYS                                 
025900     .                                                                    
026100     SKIP3                                                                
026200 MFS-RENSA-FAELT-IN SECTION.                                              
026300                                                                          
026400*    --- ALLA INDATA-FÄLT                                                 
026500     MOVE MFS-RENSA-FAELT TO MOD-IDKONTO-IN                               
026600                             MOD-IDPRCTR-IN                               
026610                             MOD-IDANALYS                                 
026700     .                                                                    
026800     EJECT                                                                
026900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027000                                                                          
027100*    --- ALLA UTDATA-FÄLT                                                 
027300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKONTO-UT                             
027400                               MOD-IDPRCTR-UT                             
027500                               MOD-IDANALYS                               
027510                               MOD-IDFTG-UT                               
027600     .                                                                    
027700     SKIP3                                                                
027800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027900                                                                          
028000*    --- ALLA INDATA-FÄLT                                                 
028100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANALYS                               
028300     .                                                                    
028400     EJECT                                                                
028500 MFS-FORM-ATTR SECTION.                                                   
028600                                                                          
028700*    --- ALLA INDATA-FÄLT                                                 
028800     MOVE MFS-FORMATETS-ATTR TO MOD-IDANALYS-ATTR                         
029000     .                                                                    
029100     SKIP2                                                                
029200 MFS-LAES-IN-IGEN SECTION.                                                
029300                                                                          
029400*    --- ALLA INDATA-FÄLT                                                 
029500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANALYS-ATTR                      
029700     .                                                                    
029800     EJECT                                                                
029900* --- IMS SEKTIONER ---                                                   
030000     SKIP3                                                                
030100 IMS-GET-MSG SECTION.                                                     
030200                                                                          
030300     MOVE '  QC' TO GODK-STATUSKODER                                      
030400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
030800     SKIP3                                                                
030900 IMS-INSERT-MSG SECTION.                                                  
031000                                                                          
031400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031500     MOVE SPACE TO GODK-STATUSKODER                                       
031600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031800     PERFORM IMS-STATUSKONTROLL                                           
031900     .                                                                    
032001     EJECT                                                                
032002 IMS-INSERT-ALT-MSG-5223 SECTION.                                         
032003                                                                          
032004     MOVE SPACE TO GODK-STATUSKODER                                       
032005     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-5223               
032006     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032007     PERFORM IMS-STATUSKONTROLL                                           
032008     .                                                                    
032009     EJECT                                                                
032010 IMS-GU-WDGX5121 SECTION.                                                 
032011                                                                          
032012     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
032013          DELIMITED BY SIZE INTO SSA1                                     
032014     MOVE '  GE' TO GODK-STATUSKODER                                      
032015     CALL CBLTDLI USING GU 5121-PCB DLI-IO-WDGX5121 SSA1                  
032016     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
032017     PERFORM IMS-STATUSKONTROLL                                           
032018     .                                                                    
032019     SKIP3                                                                
032020 IMS-GHU-WDGX5122 SECTION.                                                
032021                                                                          
032022     STRING 'WDGX5122(KEY5122  =' W-KEY5122-X ')'                         
032023          DELIMITED BY SIZE INTO SSA1                                     
032024     MOVE '  GE' TO GODK-STATUSKODER                                      
032025     CALL CBLTDLI USING GHU 5121-PCB DLI-IO-WDGX5122 SSA1                 
032026     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
032027     PERFORM IMS-STATUSKONTROLL                                           
032028     .                                                                    
032029     SKIP3                                                                
032030 IMS-ISRT-WDGX5122 SECTION.                                               
032031                                                                          
032032     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
032033          DELIMITED BY SIZE INTO SSA1                                     
032034     MOVE 'WDGX5122 ' TO SSA2                                             
032035     MOVE '  II' TO GODK-STATUSKODER                                      
032036     CALL CBLTDLI USING ISRT 5121-PCB DLI-IO-WDGX5122 SSA1 SSA2           
032037     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
032038     PERFORM IMS-STATUSKONTROLL                                           
032039     .                                                                    
032040     SKIP3                                                                
032041 IMS-REPL-WDGX5122 SECTION.                                               
032042                                                                          
032043     MOVE '  ' TO GODK-STATUSKODER                                        
032044     CALL CBLTDLI USING REPL 5121-PCB DLI-IO-WDGX5122                     
032045     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
032046     PERFORM IMS-STATUSKONTROLL                                           
032050     .                                                                    
032100     EJECT                                                                
032200 IMS-STATUSKONTROLL SECTION.                                              
032300                                                                          
032400     SET STATUS-IX TO 1                                                   
032500     SEARCH GODK-STATUS                                                   
032600       AT END                                                             
032700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032800         DELIMITED BY SIZE INTO FELTEXT                                   
032900         CALL FELLOG                                                      
033000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033100         CONTINUE                                                         
033200     END-SEARCH                                                           
033300     .                                                                    
