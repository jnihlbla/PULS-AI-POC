000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2014500.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   05/01/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HELGDAGAR PER LANDKOD                                            
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDF3                                       
001100*        PROGRAMME  UPDATES    WDF3                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T145                                              
001500*                     W2T145U                                             
001600*        MID:         W2I14501                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W2O14501                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W2014500'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400*                                                                         
003500 01  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
003600 01  FILLER REDEFINES DAGENS-DATUM-SEKEL.                                 
003700     03 WS-DATUM-AAAA            PIC 9(4).                                
003800     03 WS-DATUM-MM              PIC 9(2).                                
003900     03 WS-DATUM-DD              PIC 9(2).                                
004000*                                                                         
004100 01  WS-DATUM-NY.                                                         
004200     03 WS-DATUM-NY-AAAA         PIC 9(4)    VALUE ZERO.                  
004300     03 WS-DATUM-NY-MM           PIC 9(2)    VALUE ZERO.                  
004400     03 WS-DATUM-NY-DD           PIC 9(2)    VALUE ZERO.                  
004500*                                                                         
004600 01  WS-DATUM-NY-REDEF           PIC 9(8)    VALUE ZERO.                  
004700 01  FILLER REDEFINES WS-DATUM-NY-REDEF.                                  
004800     03 WS-DATUM-NY-CC           PIC 9(2).                                
004900     03 WS-DATUM-NY-AAMMDD       PIC 9(6).                                
005000*                                                                         
005100 01  WS-DATUM-AAAA-PLUS-FIVE     PIC 9(4)    VALUE ZERO.                  
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400 01  WORK.                                                                
005500     03 IX-RAD                   PIC S9(3)  COMP-3  VALUE ZERO.           
005600     03 RAD-MAX                  PIC S9(3)  COMP-3  VALUE 20.             
005700     03 WS-TEMFSFEL              PIC X(40)   VALUE SPACE.                 
005800     03 WS-TEMFSINF              PIC X(40)   VALUE SPACE.                 
005900     03 WS-IDLANDX2              PIC X(2)    VALUE SPACE.                 
006000     03 WS-DADATUM-HELG-NY       PIC 9(8)    VALUE ZERO.                  
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-FEL                          VALUE 'N'.                   
006500                                                                          
006600 77  INPUT-SW                    PIC X       VALUE 'N'.                   
006700     88  INPUT-JA                            VALUE 'J'.                   
006800     88  INPUT-NEJ                           VALUE 'N'.                   
006900                                                                          
007000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007100     88  NYCKLAR-OK                          VALUE 'J'.                   
007200     88  NYCKLAR-FEL                         VALUE 'N'.                   
007300                                                                          
007400 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
007500     88  UPD-RAD-JA                          VALUE 'J'.                   
007600     88  UPD-RAD-NEJ                         VALUE 'N'.                   
007700                                                                          
007800 77  NY-RAD-SW                   PIC X       VALUE 'N'.                   
007900     88  NY-RAD-JA                           VALUE 'J'.                   
008000     88  NY-RAD-NEJ                          VALUE 'N'.                   
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '2145'.                
008400     88  GODK-MID                            VALUE '2141' '2142'          
008500                                                   '2143' '2144'          
008600                                                   '2145' '2146'          
008700                                                   '2147' '2148'          
008800                                                   '2149'.                
008900     88  HELP-MID                            VALUE '0551'.                
009000     EJECT                                                                
009100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009200 01  GENERELLA-SUBPROGRAM.                                                
009300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009710     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010000*01 -COPY WMEDAREA                                                        
010100     SKIP3                                                                
010200 01  MESSAGE-CODES.                                                       
010300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010500     03  CONFLICT                PIC X(3)    VALUE '002'.                 
010600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
           03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
           03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010900     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
           03  ERR-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
011000     EJECT                                                                
011100 01  FELTEXTER.                                                           
011200     03  MED-1                  PIC X(40)                                 
011300         VALUE 'ENTER B OR D TO DELETE        '.                          
011400     03  MED-2                  PIC X(40)                                 
011500         VALUE 'INVALID DATE                  '.                          
011600     03  MED-3                  PIC X(40)                                 
011700         VALUE 'UPDATE NOT ALLOWED DATE EXISTS'.                          
011800     03  MED-4                  PIC X(40)                                 
011900         VALUE 'DATE ONLY WITHIN 5 YEARS ALLOWED'.                        
012000     03  MED-5                  PIC X(40)                                 
012100         VALUE 'NO LINES TO DELETE            '.                          
012110     03  MED-6                  PIC X(40)                                 
012120         VALUE 'NO DATES FOR THIS COUNTRY     '.                          
012200                                                                          
012300     EJECT                                                                
012310*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
012320*                                                                         
012330 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
012340     SKIP3                                                                
012350*01 -COPY WISOLAND                                                        
012360     EJECT                                                                
012400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012700     SKIP3                                                                
012800*01 -COPY WMSGINIT                                                        
012900     EJECT                                                                
013000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
013100 03  FILLER  REDEFINES MSGI-SPAR-AREA.                                    
013200     05  MSGX-IDLANDX2          PIC X(2).                                 
013300*                                                                         
013400 01  SPAR-AREA.                                                           
013500     03  SPAR-IDLANDX2          PIC X(2)    VALUE SPACE.                  
           03  FILLER                 PIC X(2)    VALUE SPACE.                  
           03  SPAR-F8-START-DATE     PIC 9(8)    VALUE ZERO.                   
           03  SPAR-SAME-PG-DATE      PIC 9(8)    VALUE ZERO.                   
           03  SPAR-PREV-PG-DATE      PIC 9(8)    VALUE ZERO.                   
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
013800     SKIP3                                                                
013900*01 -COPY WDATAREA                                                        
014000     EJECT                                                                
014100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014400     SKIP3                                                                
014500*01  MID -COPY W2I14501                                                   
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014800     SKIP3                                                                
014900*01  -COPY WMSGAREA                                                       
015000     EJECT                                                                
015100     03  MOD REDEFINES MSG-AREA.                                          
015200*      05  -COPY W2O14501                                                 
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015500     SKIP3                                                                
015600*01  -COPY WMFSAREA                                                       
015700     EJECT                                                                
015800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016100     SKIP3                                                                
016200 01  NYCKLAR-TILL-DLI.                                                    
016300     03  W-WDF3A1KY-MAX-X.                                                
016400         05  W-IDLANDX2-MAX      PIC X(2).                                
016500         05  W-DADATUM-HELG-MAX  PIC 9(8)     VALUE 99999999.             
016600     03  W-WDF3A1KY-MIN-X.                                                
016700         05  W-IDLANDX2-MIN      PIC X(2).                                
016800         05  W-DADATUM-HELG-MIN  PIC 9(8)     VALUE ZERO.                 
016900     03  W-WDF301KY-X.                                                    
017000         05  W-IDLANDX2-NY       PIC X(2).                                
017100         05  W-DADATUM-HELG-NY   PIC 9(8)     VALUE ZERO.                 
017200     SKIP2                                                                
017300*    --- STATUS-KOD FRÅN IMS                                              
017400 01  STATUS-WS                   PIC XX.                                  
017500     88  SEGMENT-FINNS                       VALUE '  '.                  
017600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017900     SKIP2                                                                
018000 01  GODK-STATUSKODER.                                                    
018100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018200     SKIP3                                                                
018300 01  SSA1                        PIC X(64).                               
018400 01  SSA2                        PIC X(64).                               
018500     EJECT                                                                
018600*    --- IMS FUNKTIONSKODER                                               
018700*01  -COPY W0003                                                          
018800     EJECT                                                                
018900*    ---  DLI INPUT-OUTPUT AREA                                           
019000                                                                          
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF3A1'.                      
019200 01  DLI-IO-WDF3A1.                                                       
019300*    03  -COPY WDF3A1                                                     
019400     EJECT                                                                
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF301'.                      
019600 01  DLI-IO-WDF301.                                                       
019700*    03  -COPY WDF301                                                     
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000*01  -COPY W0009   -PRE MSG-                                              
020100*01  -COPY W0008   -PRE USEA-                                             
020200     05  FILLER                  PIC X.                                   
020300                                                                          
020400*01  -COPY W0008  -PRE WDF3A-                                             
020500     05  FILLER                  PIC X.                                   
020600*01  -COPY W0008  -PRE WDF3-                                              
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDF3A-PCB WDF3-PCB.           
021000 MAIN SECTION.                                                            
021100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDF3A-PCB WDF3-PCB.           
021200                                                                          
021300     PERFORM IMS-GET-MSG                                                  
021400     IF SEGMENT-FINNS                                                     
021500       PERFORM A-INIT                                                     
021600       PERFORM B-KOLLA-NYCKLAR                                            
021700       IF NYCKLAR-OK                                                      
021800           IF MFS-UPDATE                                                  
021900              PERFORM G-KOLLA-INPUT                                       
022000              IF INDATA-OK                                                
022100                 PERFORM H-UPPDATERA                                      
022200              END-IF                                                      
022300           ELSE                                                           
022400             IF MFS-FIRST                                                 
022500                PERFORM C-FOERSTA-SIDA                                    
022600             ELSE                                                         
                      IF MFS-NEXT                                               
                         PERFORM D-NEXT-PAGE                                    
                      ELSE                                                      
022700                   PERFORM E-SAMMA-SIDA                                   
                      END-IF                                                    
022800             END-IF                                                       
022900           END-IF                                                         
023000           PERFORM F-LAES-VISA-INFO                                       
023100       END-IF                                                             
023200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O14501 + 4                      
023300       PERFORM IMS-INSERT-MSG                                             
023400     END-IF                                                               
023500                                                                          
023600     MOVE ZERO TO RETURN-CODE                                             
023700     GOBACK                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 A-INIT SECTION.                                                          
024100                                                                          
024200     IF MSG-DUBBLA-TRANSKODER                                             
024300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I14501                 
024400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024600     ELSE                                                                 
024700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I14501                  
024800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
024900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025000     END-IF                                                               
025100                                                                          
025200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
025300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025500                                                                          
025600     MOVE LOW-VALUE TO MSG-AREA                                           
025700     MOVE 'W2O145N1' TO MFS-IDMOD                                         
025800     MOVE '2145' TO MOD-IDTRANS                                           
025900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
026000                                                                          
026100     IF EGEN-MID OR HELP-MID                                              
026200       CONTINUE                                                           
026300     ELSE                                                                 
026400       MOVE SPACE TO MFS-KDTRTYP                                          
026500       MOVE '7' TO MFS-IDPFK                                              
026600     END-IF                                                               
026700*                                                                         
026800*    GET CURRENT DATE AND FIVE YEARS AHEAD                                
026900     MOVE FUNCTION CURRENT-DATE (1:8)                                     
027000                                 TO DAGENS-DATUM-SEKEL                    
027100     COMPUTE WS-DATUM-AAAA-PLUS-FIVE                                      
027200                                  = WS-DATUM-AAAA + 5                     
027300*                                                                         
027400     MOVE 'GB '                  TO MED-IDSKYLT                           
027500     MOVE SPACE                  TO WS-TEMFSFEL                           
027600                                    WS-TEMFSINF                           
027700     .                                                                    
027800     EJECT                                                                
027900 B-KOLLA-NYCKLAR SECTION.                                                 
028000                                                                          
028100     MOVE ALL '+'                TO MSGI-WMSGINIT                         
028200     MOVE '001'                  TO MSGI-KDCALL                           
028300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
028400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
028500     MOVE '2145'                 TO MSGI-IDTRANS                          
028600     IF EGEN-MID                                                          
028700         MOVE MID-IDLANDX2-IN    TO MSGX-IDLANDX2                         
028800                                    MSGI-IDLANDX2                         
028900     END-IF                                                               
029000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029100     MOVE MSGI-SPAR-AREA         TO SPAR-AREA                             
029200                                                                          
029300*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
029400     MOVE 'GB  '                 TO MED-IDSKYLT                           
029500                                                                          
029600     MOVE JA TO NYCKLAR-SW                                                
029700                                                                          
029800*    -- KONTROLL AV IDLANDX2                                              
029900     MOVE MFS-RENSA-FAELT        TO MOD-IDLANDX2-IN                       
030000                                                                          
030100     MOVE MSGI-IDLANDX2          TO WS-IDLANDX2                           
030200                                                                          
030300     IF MID-IDLANDX2-IN           = ALL '+'                               
030400        CONTINUE                                                          
030500     ELSE                                                                 
030600        MOVE '7'                 TO MFS-IDPFK                             
030700        MOVE SPACE               TO MFS-KDTRTYP                           
030800     END-IF                                                               
030900                                                                          
031000     IF WS-IDLANDX2     >= 'AA' AND                                       
031100        WS-IDLANDX2     <= 'ZZ'                                           
031200        CONTINUE                                                          
031300     ELSE                                                                 
031400        MOVE 'SE'                TO WS-IDLANDX2                           
031500     END-IF                                                               
031510                                                                          
031520     PERFORM S01-VALIDATE-COUNTRY                                         
031600                                                                          
031700     MOVE WS-IDLANDX2            TO W-IDLANDX2-MIN                        
031800                                    W-IDLANDX2-MAX                        
031900                                    W-IDLANDX2-NY                         
032000*                                                                         
032100     IF GODK-MID OR NYCKLAR-OK                                            
032200       MOVE WS-IDLANDX2          TO MOD-IDLANDX2-UT                       
032300     ELSE                                                                 
032400       MOVE MFS-RENSA-FAELT      TO MOD-IDLANDX2-UT                       
032500     END-IF                                                               
032600                                                                          
032700     IF NYCKLAR-FEL                                                       
032800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
032900       CALL WMEDKONV USING MED-WMEDAREA                                   
033000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
033100       PERFORM MFS-RENSA-FAELT-UT                                         
033200       PERFORM MFS-RENSA-FAELT-IN                                         
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 C-FOERSTA-SIDA SECTION.                                                  
033700                                                                          
033800     PERFORM MFS-RENSA-FAELT-IN                                           
      *RESET USER DATABASE VALUES                                               
           MOVE ZERO        TO SPAR-F8-START-DATE                               
                               SPAR-SAME-PG-DATE                                
                               SPAR-PREV-PG-DATE                                
033900     .                                                                    
034000     EJECT                                                                
       D-NEXT-PAGE SECTION.                                                     
                                                                                
           IF SPAR-F8-START-DATE = ZERO                                         
              MOVE ERR-LAST-PAGE-SHOWN   TO MED-IDMFSFEL                        
              CALL WMEDKONV USING MED-WMEDAREA                                  
              MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
           END-IF                                                               
           PERFORM MFS-RENSA-FAELT-IN                                           
           .                                                                    
           EJECT                                                                
034100 E-SAMMA-SIDA SECTION.                                                    
034200                                                                          
034300     PERFORM S02-CHECK-INPUT                                              
034400*                                                                         
034500     IF INPUT-NEJ                                                         
034600        PERFORM MFS-RENSA-FAELT-IN                                        
034700     ELSE                                                                 
034800       IF EGEN-MID OR HELP-MID                                            
034900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
035000         CALL WMEDKONV USING MED-WMEDAREA                                 
035100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
035200         PERFORM MFS-LAES-IN-IGEN                                         
035300                                                                          
035400         PERFORM EA-MID-INDATA-TILL-MOD                                   
035500       ELSE                                                               
035600         PERFORM MFS-RENSA-FAELT-IN                                       
035700       END-IF                                                             
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 EA-MID-INDATA-TILL-MOD SECTION.                                          
036200                                                                          
036300     MOVE +1                     TO IX-RAD                                
036400     PERFORM UNTIL IX-RAD         > RAD-MAX                               
036500       IF MID-CMD (IX-RAD)        = ALL '+'                               
036600         MOVE MFS-RENSA-FAELT                                             
036700                                 TO MOD-CMD (IX-RAD)                      
036800       ELSE                                                               
036900         MOVE MID-CMD (IX-RAD)   TO MOD-CMD (IX-RAD)                      
037000       END-IF                                                             
037100       ADD +1                    TO IX-RAD                                
037200     END-PERFORM                                                          
037300                                                                          
037400     IF MID-DADATUM-HELG-NY       = ALL '+'                               
037500         MOVE MFS-RENSA-FAELT                                             
037600                                 TO MOD-DADATUM-HELG-NY                   
037700     ELSE                                                                 
037800         MOVE MID-DADATUM-HELG-NY                                         
037900                                 TO MOD-DADATUM-HELG-NY                   
038000     END-IF                                                               
038100                                                                          
038200     .                                                                    
038300     EJECT                                                                
038400 F-LAES-VISA-INFO SECTION.                                                
038500                                                                          
      **MFS-NEXT                    -  TO GO TO NEXT SCREEN                     
      **UPD-RAD-JA AND NY-RAD-NEJ   -  FLAGS FOR DELETE A ENTRY                 
      **SPAR-F8-START-DATE          -  TO STORE 21ST ENTRY                      
      **SPAR-SAME-PG-DATE           -  TO LOAD THE SAME PAGE AGAIN              
      **                               DURING DELETION,AFTER REACH THE          
      **                               LAST PAGE                                
      **WS-DADATUM-HELG-NY          -  TO LOAD THE FIRST PAGE                   
           IF MFS-NEXT OR                                                       
              ( UPD-RAD-JA AND NY-RAD-NEJ)                                      
              IF SPAR-F8-START-DATE IS ZERO OR                                  
                 ( UPD-RAD-JA AND NY-RAD-NEJ)                                   
                 MOVE SPAR-SAME-PG-DATE TO W-DADATUM-HELG-MIN                   
              ELSE                                                              
                 MOVE SPAR-F8-START-DATE  TO W-DADATUM-HELG-MIN                 
                 MOVE SPAR-SAME-PG-DATE   TO SPAR-PREV-PG-DATE                  
              END-IF                                                            
           ELSE                                                                 
038600        MOVE WS-DADATUM-HELG-NY  TO W-DADATUM-HELG-MIN                    
           END-IF                                                               
038700*                                                                         
038800     PERFORM IMS-GET-WDF3A1-FORST                                         
038900*IF ALL ENTRIES IN THE PAGE ARE DELETED ,TO BACK TO THE PREV PAGE         
           IF ( UPD-RAD-JA AND NY-RAD-NEJ)                                      
              IF SEGMENT-SAKNAS                                                 
                 MOVE SPAR-PREV-PG-DATE TO                                      
                                       W-DADATUM-HELG-MIN                       
                 PERFORM IMS-GET-WDF3A1-FORST                                   
              END-IF                                                            
           END-IF                                                               
      *                                                                         
           MOVE W-DADATUM-HELG-MIN     TO SPAR-SAME-PG-DATE                     
039000     IF SEGMENT-SAKNAS                                                    
039100        MOVE MED-6               TO WS-TEMFSFEL                           
039310        MOVE WS-TEMFSFEL         TO MOD-TEMFSFEL                          
039320        PERFORM S01A-CLOSE-FIELD                                          
039400        PERFORM MFS-RENSA-FAELT-UT                                        
039500     ELSE                                                                 
039600        MOVE +1                  TO IX-RAD                                
039700        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
039800          OR IX-RAD > 20                                                  
039900          MOVE HLGA-DADATUM-HELG TO MOD-DADATUM-HELG (IX-RAD)             
040000*         --- HIGHLIGHT NEW HOLIDAY INSERTED                              
040100          IF HLGA-DADATUM-HELG    = WS-DADATUM-HELG-NY                    
040200             MOVE MFS-ADD-LYS-UPP-FAELT                                   
040300                                 TO MOD-DADATUM-HELG-ATTR (IX-RAD)        
040400          END-IF                                                          
040500          ADD +1                 TO IX-RAD                                
040600          PERFORM IMS-GET-WDF3A1-NASTA                                    
040700        END-PERFORM                                                       
040800                                                                          
      *TO STORE THE 21ST ENTRY IN USER DATABASE.SO IT CAN BE REFERRED           
      *LATER FOR F8 FUNCTION                                                    
              IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                 
                 MOVE ZERO             TO SPAR-F8-START-DATE                    
              ELSE                                                              
                 MOVE HLGA-DADATUM-HELG   TO SPAR-F8-START-DATE                 
              END-IF                                                            
              MOVE '002'               TO MSGI-KDCALL                           
              MOVE '2145'              TO SPAR-AREA(1:4)                        
                                                                                
              MOVE SPAR-AREA           TO MSGI-SPAR-AREA                        
              CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
      *                                                                         
      *TO DISPLAY THE INFORMATION MESSAGE                                       
              IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                 
                 MOVE INF-LAST-PAGE        TO MED-IDMFSINF                      
              ELSE                                                              
                 MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
              END-IF                                                            
              CALL WMEDKONV USING MED-WMEDAREA                                  
              MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
      *                                                                         
040900        PERFORM UNTIL IX-RAD > 20                                         
041000           MOVE ZERO             TO MOD-DADATUM-HELG (IX-RAD)             
041100           MOVE MFS-STAENG-FAELT                                          
041200                                 TO MOD-CMD-ATTR (IX-RAD)                 
041300           ADD +1                TO IX-RAD                                
041400        END-PERFORM                                                       
041500     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 G-KOLLA-INPUT    SECTION.                                                
041900                                                                          
042000     MOVE JA                     TO INDATA-SW                             
042100     MOVE NEJ                    TO UPD-RAD-SW                            
042200                                    NY-RAD-SW                             
042300     MOVE ZERO                   TO WS-DADATUM-HELG-NY                    
042400                                                                          
042500     PERFORM S02-CHECK-INPUT                                              
042600                                                                          
042700     IF INPUT-NEJ                                                         
042800        MOVE ERR-PF11-AND-NO-DATA                                         
042900                                 TO MED-IDMFSFEL                          
043000        MOVE NEJ                 TO INDATA-SW                             
043100     ELSE                                                                 
043200        MOVE +1                  TO IX-RAD                                
043300        PERFORM UNTIL IX-RAD      > RAD-MAX                               
043400          IF MID-CMD (IX-RAD) NOT = ALL '+'                               
043500           IF MID-CMD (IX-RAD)    = 'B'                                   
043600           OR MID-CMD (IX-RAD)    = 'D'                                   
043700              IF MID-DADATUM-HELG (IX-RAD)                                
043800                                  > ZERO                                  
043900                 MOVE MFS-ALFA-FAELT-RAETT                                
044000                                 TO MOD-CMD-ATTR (IX-RAD)                 
044100                 MOVE JA         TO UPD-RAD-SW                            
044200              ELSE                                                        
044300                 MOVE MFS-ALFA-FAELT-FEL                                  
044400                                 TO MOD-CMD-ATTR (IX-RAD)                 
044500                 MOVE ERR-CORR-HILITE-FLDS                                
044600                                 TO MED-IDMFSFEL                          
044700                 MOVE MED-5      TO WS-TEMFSINF                           
044800                 MOVE NEJ        TO INDATA-SW                             
044900              END-IF                                                      
045000           ELSE                                                           
045100              MOVE MFS-ALFA-FAELT-FEL                                     
045200                                 TO MOD-CMD-ATTR (IX-RAD)                 
045300              MOVE ERR-CORR-HILITE-FLDS                                   
045400                                 TO MED-IDMFSFEL                          
045500              MOVE MED-1         TO WS-TEMFSINF                           
045600              MOVE NEJ           TO INDATA-SW                             
045700           END-IF                                                         
045800          ELSE                                                            
045900              MOVE MFS-RENSA-FAELT                                        
046000                                 TO MOD-CMD-ATTR (IX-RAD)                 
046100          END-IF                                                          
046200          ADD +1                 TO IX-RAD                                
046300        END-PERFORM                                                       
046400*                                                                         
046500        IF  MID-DADATUM-HELG-NY NOT = ALL '+'                             
046501        AND MID-DADATUM-HELG-NY     > ZERO                                
046502*                                                                         
046520*                                                                         
046600           MOVE MID-DADATUM-HELG-NY                                       
046700                                 TO WS-DATUM-NY                           
046800                                    WS-DATUM-NY-REDEF                     
046900           IF  WS-DATUM-NY        > DAGENS-DATUM-SEKEL                    
047000*             --- VALIDATE USER ENTERDED DATE                             
047100              MOVE 'AAMMDD'                                               
047200                                 TO DAT-KDDATFORM                         
047300              MOVE WS-DATUM-NY-AAMMDD                                     
047400                                 TO DAT-I-TIDATUM                         
047500                                                                          
047600              CALL WDATKONV   USING DAT-KDDATFORM DAT-I-TIDATUM           
047700                                    DAT-O-TIDATUM DAT-KDSVAR              
047800              IF  DAT-KDSVAR-OK                                           
047900              AND WS-DATUM-NY-AAAA                                        
048000                              NOT > WS-DATUM-AAAA-PLUS-FIVE               
048100                  MOVE MFS-NUM-FAELT-RAETT                                
048200                                 TO MOD-DADATUM-HELG-NY-ATTR              
048300                  MOVE JA        TO NY-RAD-SW                             
048400              ELSE                                                        
048500                  IF WS-DATUM-NY-AAAA                                     
048600                                  > WS-DATUM-AAAA-PLUS-FIVE               
048700                     MOVE MED-4  TO WS-TEMFSINF                           
048800                  ELSE                                                    
048900                     MOVE MED-2  TO WS-TEMFSINF                           
049000                  END-IF                                                  
049100                  MOVE MFS-NUM-FAELT-FEL                                  
049200                                 TO MOD-DADATUM-HELG-NY-ATTR              
049300                  MOVE ERR-CORR-HILITE-FLDS                               
049400                                 TO MED-IDMFSFEL                          
049500                  MOVE NEJ       TO INDATA-SW                             
049600                  MOVE MID-DADATUM-HELG-NY                                
049700                                 TO MOD-DADATUM-HELG-NY                   
049800              END-IF                                                      
049900           ELSE                                                           
050000              MOVE MFS-NUM-FAELT-FEL                                      
050100                                 TO MOD-DADATUM-HELG-NY-ATTR              
050200              MOVE ERR-CORR-HILITE-FLDS                                   
050300                                 TO MED-IDMFSFEL                          
050400              MOVE MED-2         TO WS-TEMFSINF                           
050500              MOVE NEJ           TO INDATA-SW                             
050600              MOVE MID-DADATUM-HELG-NY                                    
050700                                 TO MOD-DADATUM-HELG-NY                   
050800           END-IF                                                         
050900        END-IF                                                            
051000     END-IF                                                               
051100                                                                          
051200     IF NY-RAD-JA                                                         
051300        MOVE MID-DADATUM-HELG-NY                                          
051400                                 TO W-DADATUM-HELG-NY                     
051500        PERFORM IMS-GU-WDF301                                             
051600        IF SEGMENT-FINNS                                                  
051700           MOVE MFS-NUM-FAELT-FEL                                         
051800                                 TO MOD-DADATUM-HELG-NY-ATTR              
051900           MOVE MED-3            TO WS-TEMFSFEL                           
052000           MOVE NEJ              TO INDATA-SW                             
052100        END-IF                                                            
052200     END-IF                                                               
052300                                                                          
052400     IF INDATA-FEL                                                        
052500        IF WS-TEMFSFEL            = SPACE                                 
052600          CALL WMEDKONV       USING MED-WMEDAREA                          
052700          MOVE MED-TEMFSFEL      TO MOD-TEMFSFEL                          
052800        ELSE                                                              
052900          MOVE WS-TEMFSFEL       TO MOD-TEMFSFEL                          
053000        END-IF                                                            
053100        IF WS-TEMFSINF            > SPACES                                
053200          MOVE WS-TEMFSINF       TO MOD-TEMFSINF                          
053300        END-IF                                                            
053400        PERFORM MFS-ROER-EJ-FAELT-IN                                      
053500     ELSE                                                                 
053600        IF  UPD-RAD-JA                                                    
053700        AND NY-RAD-JA                                                     
053800*                                                                         
053900*         --- SIMUTANEOUSLY DATE MARKED FOR DELETION AND                  
054000*         --- NEW DATE ENTERED FOR INSERTION - ERROR                      
054100*                                                                         
054200          MOVE NEJ               TO INDATA-SW                             
054300          MOVE CONFLICT          TO MED-IDMFSFEL                          
054400          CALL WMEDKONV       USING MED-WMEDAREA                          
054500          MOVE MED-TEMFSFEL      TO MOD-TEMFSFEL                          
054501          MOVE +1                TO IX-RAD                                
054502          PERFORM UNTIL IX-RAD    > RAD-MAX                               
054503            IF MID-CMD (IX-RAD)                                           
054504                             NOT  = ALL '+'                               
054505            AND MID-CMD (IX-RAD)  > SPACES                                
054506                MOVE MFS-ALFA-FAELT-FEL                                   
054507                                 TO MOD-CMD-ATTR (IX-RAD)                 
054508            END-IF                                                        
054509            ADD +1               TO IX-RAD                                
054510          END-PERFORM                                                     
054511          MOVE MFS-NUM-FAELT-FEL                                          
054520                                 TO MOD-DADATUM-HELG-NY-ATTR              
054600          PERFORM MFS-ROER-EJ-FAELT-IN                                    
054700        END-IF                                                            
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100 H-UPPDATERA SECTION.                                                     
055200                                                                          
055300     IF UPD-RAD-JA                                                        
055400        MOVE +1                  TO  IX-RAD                               
055500        PERFORM UNTIL IX-RAD      > RAD-MAX                               
055600          IF MID-CMD (IX-RAD)    = 'B' OR 'D'                             
055700             MOVE MID-DADATUM-HELG (IX-RAD)                               
055800                                 TO W-DADATUM-HELG-NY                     
055900             PERFORM IMS-GHU-WDF301                                       
056000             PERFORM IMS-DLET-WDF301                                      
056100          END-IF                                                          
056200          ADD +1                 TO IX-RAD                                
056300        END-PERFORM                                                       
056400     ELSE                                                                 
056500        IF NY-RAD-JA                                                      
056600           MOVE WS-IDLANDX2      TO HLG-IDLANDX2                          
056700           MOVE MID-DADATUM-HELG-NY                                       
056800                                 TO HLG-DADATUM-HELG                      
056900                                    WS-DADATUM-HELG-NY                    
057000           MOVE 'J'              TO HLG-FLHELG                            
057100           PERFORM IMS-ISRT-WDF301                                        
057200        END-IF                                                            
057300     END-IF                                                               
057400                                                                          
057500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
057600     CALL WMEDKONV USING MED-WMEDAREA                                     
057700     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
057800     PERFORM MFS-RENSA-FAELT-IN                                           
057900     .                                                                    
058000     EJECT                                                                
058100 S01-VALIDATE-COUNTRY SECTION.                                            
058101                                                                          
058102     MOVE WS-IDLANDX2            TO LAND-IDLANDX2                         
058103     MOVE SPACE                  TO LAND-IDLANDX3                         
058104     CALL WISOLAND USING LAND-WISOLAND                                    
058105     IF LAND-KDSVAR = SPACE                                               
058106        CONTINUE                                                          
058107     ELSE                                                                 
058108        PERFORM S01A-CLOSE-FIELD                                          
058109        MOVE MFS-STAENG-FAELT    TO MOD-DADATUM-HELG-NY-ATTR              
058110        MOVE NEJ                 TO NYCKLAR-SW                            
058111     END-IF                                                               
058112     .                                                                    
058113     EJECT                                                                
058114 S01A-CLOSE-FIELD SECTION.                                                
058115                                                                          
058116     MOVE +1                     TO IX-RAD                                
058117     PERFORM UNTIL IX-RAD > RAD-MAX                                       
058118       MOVE MFS-STAENG-FAELT     TO MOD-CMD-ATTR (IX-RAD)                 
058119       ADD +1                    TO IX-RAD                                
058120     END-PERFORM                                                          
058121     .                                                                    
058122     EJECT                                                                
058130 S02-CHECK-INPUT    SECTION.                                              
058200                                                                          
058300     MOVE NEJ                    TO INPUT-SW                              
058400     MOVE +1                     TO IX-RAD                                
058500     PERFORM UNTIL IX-RAD         > RAD-MAX                               
058600       IF MID-CMD (IX-RAD)    NOT = ALL '+'                               
058700          IF MID-CMD (IX-RAD)     > SPACES                                
058800             MOVE JA             TO INPUT-SW                              
058900             MOVE RAD-MAX        TO IX-RAD                                
059000          ELSE                                                            
059100             MOVE ALL '+'        TO MID-CMD (IX-RAD)                      
059200          END-IF                                                          
059300       END-IF                                                             
059400       ADD +1                    TO IX-RAD                                
059500     END-PERFORM                                                          
059600*                                                                         
059610     INSPECT MID-DADATUM-HELG-NY REPLACING ALL SPACE BY ZERO              
059620*                                                                         
059700     IF  MID-DADATUM-HELG-NY  NOT = ALL '+'                               
059710     AND MID-DADATUM-HELG-NY      > ZERO                                  
059800     AND INPUT-NEJ                                                        
059900         MOVE JA                 TO INPUT-SW                              
060000     END-IF                                                               
060100*                                                                         
060200     .                                                                    
060300     EJECT                                                                
060400 MFS-RENSA-FAELT-UT SECTION.                                              
060500                                                                          
060600*    --- ALLA UTDATA-FÄLT                                                 
060700     MOVE +1                     TO IX-RAD                                
060800     PERFORM UNTIL IX-RAD > 20                                            
060900        MOVE MFS-RENSA-FAELT     TO MOD-DADATUM-HELG (IX-RAD)             
061000        ADD +1                   TO IX-RAD                                
061100     END-PERFORM                                                          
061200     .                                                                    
061300     EJECT                                                                
061400 MFS-RENSA-FAELT-IN SECTION.                                              
061500                                                                          
061600*    --- ALLA INDATA-FÄLT                                                 
061700     MOVE +1                     TO IX-RAD                                
061800     PERFORM UNTIL IX-RAD         > RAD-MAX                               
061900       MOVE MFS-RENSA-FAELT      TO MOD-CMD (IX-RAD)                      
062000       ADD +1                    TO IX-RAD                                
062100     END-PERFORM                                                          
062200     MOVE MFS-RENSA-FAELT        TO MOD-DADATUM-HELG-NY                   
062300     .                                                                    
062400     EJECT                                                                
062500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
062600                                                                          
062700*    --- ALLA UTDATA-FÄLT                                                 
062800     MOVE +1                     TO IX-RAD                                
062900     PERFORM UNTIL IX-RAD         > RAD-MAX                               
063000       MOVE MFS-ROER-EJ-FAELT                                             
063100                                 TO MOD-CMD (IX-RAD)                      
063200       ADD +1                    TO IX-RAD                                
063300     END-PERFORM                                                          
063400     MOVE MFS-ROER-EJ-FAELT      TO MOD-DADATUM-HELG-NY                   
063500     .                                                                    
063600     EJECT                                                                
063700 MFS-LAES-IN-IGEN SECTION.                                                
063800                                                                          
063900*    --- ALLA INDATA-FÄLT                                                 
064000     MOVE +1                     TO IX-RAD                                
064100     PERFORM UNTIL IX-RAD         > RAD-MAX                               
064200       MOVE MFS-ADD-LAES-IN-FAELT                                         
064300                                 TO MOD-CMD-ATTR (IX-RAD)                 
064400       ADD +1                    TO IX-RAD                                
064500     END-PERFORM                                                          
064600     MOVE MFS-ADD-LAES-IN-FAELT                                           
064700                                 TO MOD-DADATUM-HELG-NY-ATTR              
064800     .                                                                    
064900     EJECT                                                                
065000 MFS-STAENG-FAELT-IN    SECTION.                                          
065100                                                                          
065200*    --- PROTECT INPUT FIELDS                                             
065300     MOVE +1                     TO IX-RAD                                
065400     PERFORM UNTIL IX-RAD         > RAD-MAX                               
065500       MOVE MFS-STAENG-FAELT     TO MOD-CMD-ATTR (IX-RAD)                 
065600                                                                          
065700       ADD +1                    TO IX-RAD                                
065800     END-PERFORM                                                          
065900     MOVE MFS-STAENG-FAELT       TO MOD-DADATUM-HELG-NY-ATTR              
066000     .                                                                    
066100     EJECT                                                                
066200* --- IMS SEKTIONER ---                                                   
066300     SKIP3                                                                
066400 IMS-GET-MSG SECTION.                                                     
066500                                                                          
066600     MOVE '  QC' TO GODK-STATUSKODER                                      
066700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
066800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066900     PERFORM IMS-STATUSKONTROLL                                           
067000     .                                                                    
067100     SKIP3                                                                
067200 IMS-INSERT-MSG SECTION.                                                  
067300                                                                          
067700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
067800     MOVE SPACE TO GODK-STATUSKODER                                       
067900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
068000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068100     PERFORM IMS-STATUSKONTROLL                                           
068200     .                                                                    
068300     EJECT                                                                
068400 IMS-GET-WDF3A1-FORST SECTION.                                            
068500                                                                          
068600     STRING 'WDF3A1  (WDF3A1KY=>' W-WDF3A1KY-MIN-X                        
068700                    '&WDF3A1KY <' W-WDF3A1KY-MAX-X ')'                    
068800          DELIMITED BY SIZE INTO SSA1                                     
068900     MOVE '  GE' TO GODK-STATUSKODER                                      
069000     CALL CBLTDLI USING GU WDF3A-PCB DLI-IO-WDF3A1 SSA1                   
069100     MOVE WDF3A-STATUS-CODE TO STATUS-WS                                  
069200     PERFORM IMS-STATUSKONTROLL                                           
069300     .                                                                    
069400     EJECT                                                                
069500 IMS-GET-WDF3A1-NASTA SECTION.                                            
069600                                                                          
069700     STRING 'WDF3A1  (WDF3A1KY >' W-WDF3A1KY-MIN-X                        
069800                    '&WDF3A1KY <' W-WDF3A1KY-MAX-X ')'                    
069900          DELIMITED BY SIZE INTO SSA1                                     
070000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
070100     CALL CBLTDLI USING GN WDF3A-PCB DLI-IO-WDF3A1 SSA1                   
070200     MOVE WDF3A-STATUS-CODE TO STATUS-WS                                  
070300     PERFORM IMS-STATUSKONTROLL                                           
070400     .                                                                    
070500     EJECT                                                                
070600 IMS-GU-WDF301       SECTION.                                             
070700                                                                          
070800     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
070900          DELIMITED BY SIZE INTO SSA1                                     
071000     MOVE '  GE' TO GODK-STATUSKODER                                      
071100     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-WDF301 SSA1                    
071200     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
071300     PERFORM IMS-STATUSKONTROLL                                           
071400     .                                                                    
071500     EJECT                                                                
071600 IMS-GHU-WDF301 SECTION.                                                  
071700                                                                          
071800     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
071900          DELIMITED BY SIZE INTO SSA1                                     
072000     MOVE '    ' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING GHU WDF3-PCB DLI-IO-WDF301 SSA1                   
072200     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     SKIP3                                                                
072600 IMS-ISRT-WDF301 SECTION.                                                 
072700                                                                          
072800     MOVE 'WDF301 ' TO SSA1                                               
072900     MOVE '  II' TO GODK-STATUSKODER                                      
073000     CALL CBLTDLI USING ISRT WDF3-PCB DLI-IO-WDF301 SSA1                  
073100     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
073200     PERFORM IMS-STATUSKONTROLL                                           
073300     .                                                                    
073400     SKIP3                                                                
073500 IMS-DLET-WDF301 SECTION.                                                 
073600                                                                          
073700     MOVE '  ' TO GODK-STATUSKODER                                        
073800     CALL CBLTDLI USING DLET WDF3-PCB DLI-IO-WDF301                       
073900     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
074000     PERFORM IMS-STATUSKONTROLL                                           
074100     .                                                                    
074200     EJECT                                                                
074300 IMS-STATUSKONTROLL SECTION.                                              
074400                                                                          
074500     SET STATUS-IX TO 1                                                   
074600     SEARCH GODK-STATUS                                                   
074700       AT END                                                             
074800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
074900         DELIMITED BY SIZE INTO FELTEXT                                   
075000         CALL FELLOG                                                      
075100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075200         CONTINUE                                                         
075300     END-SEARCH                                                           
075400     .                                                                    
