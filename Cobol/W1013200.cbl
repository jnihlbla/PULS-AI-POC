000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1013200.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   93/11/10.                                                
000600                                                                          
000700*MODIFIERAT 9701 AV KENT JEBSEN I SAMBAND MED NDC-PROJEKTET               
000800*                                                                         
000900*    REMARKS.                                                             
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        VISAR OCH UPPDATERAR NOTERINGS-TEXTER FÖR FARLIGT GODS           
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WL1165 (WDR2)                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W1T132                                              
001800*        MID:         W1I13201                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W1O13201                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W1013200'.            
003100                                                                          
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003800                                                                          
003900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004000     88  INDATA-OK                           VALUE 'J'.                   
004100     88  INDATA-FEL                          VALUE 'N'.                   
004200                                                                          
004300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004400     88  NYCKLAR-OK                          VALUE 'J'.                   
004500     88  NYCKLAR-FEL                         VALUE 'N'.                   
004600                                                                          
004700 77  ENTER-OCH-INDATA-SW         PIC X       VALUE 'N'.                   
004800     88  ENTER-OCH-INDATA                    VALUE 'J'.                   
004900                                                                          
005000 77  DLET-KDFGTRP-SW             PIC X       VALUE 'N'.                   
005100 77  DLET-1166-SW                PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005400     88  EGEN-MID                            VALUE '1132'.                
005500     88  GODK-MID                            VALUE '1118'.                
005600     88  HELP-MID                            VALUE '0551'.                
006020 77  UPPER-ALPHA                 PIC X(29)                                
006030                            VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.        
006040 77  LOWER-ALPHA                 PIC X(29)                                
006050                            VALUE 'abcdefghijklmnopqrstuvwxyzåäö'.        
006060     EJECT                                                                
006070 01  ARBETSFALT.                                                          
006080     03  IX                           PIC S9(1) VALUE ZERO.               
006090     03  WS-IDPSN                     PIC  9(3) VALUE ZERO.               
006091     03  WS-IDSPRAK                   PIC  X(2) VALUE SPACE.              
006100     EJECT                                                                
006200*      --- VALID IDDC CODES                                               
006300*                                                                         
006400*01    -COPY WWDC99                                                       
006500       EJECT                                                              
006600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006700 01  GENERELLA-SUBPROGRAM.                                                
006800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007110     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008200     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
008300 01  FELMEDDELANDEN.                                                      
008400     03  FEL-1                   PIC X(40)   VALUE                        
008500         'PROPER SHIPPING NAME ID MISSING        '.                       
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W1I13201                                                   
009200     EJECT                                                                
009300*01  MID -COPY W1I11801  -PRE 1118-.                                      
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W1O13201                                                 
010100     EJECT                                                                
010200*01  -COPY WMSGINIT                                                       
010210*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
010220*                                                                         
010230 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
010240     SKIP3                                                                
010250*01 -COPY WISOLAND                                                        
010260                                                                          
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010500     SKIP3                                                                
010600*01  -COPY WMFSAREA                                                       
010700     EJECT                                                                
010800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011300     03  W-1165KEY-X.                                                     
011400         05  W-1165              PIC X(4)    VALUE '1165'.                
011500         05  W-IDPSN             PIC 9(3)    VALUE ZERO.                  
011510         05  W-IDSPRAK           PIC X(2)    VALUE SPACE.                 
011600         05  FILLER              PIC X(21)   VALUE LOW-VALUE.             
011700     03  W-1168KEY-X.                                                     
011800         05  W-KDFGTRP           PIC 9(02)   VALUE ZERO.                  
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012400     SKIP2                                                                
012500 01  GODK-STATUSKODER.                                                    
012600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(64).                               
012900 01  SSA2                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013600     SKIP3                                                                
013700 01  DLI-IO-AREA.                                                         
013800     03  IO-AREA                 PIC X(250)  VALUE SPACE.                 
013900     SKIP3                                                                
014000     03  WL116501 REDEFINES IO-AREA.                                      
014100*        05  -COPY WDGX1165                                               
014200     EJECT                                                                
014300     03  WL116511 REDEFINES IO-AREA.                                      
014400*        05  -COPY WDGX1166                                               
014500     EJECT                                                                
014600     03  WL116512 REDEFINES IO-AREA.                                      
014700*        05  -COPY WDGX1168                                               
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000                                                                          
015100*01  -COPY W0009   -PRE MSG-                                              
015200     EJECT                                                                
015300*01  -COPY W0008   -PRE USEA-                                             
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600*01  -COPY W0008   -PRE 1165-                                             
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 1165-PCB.                     
016000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 1165-PCB.                     
016100                                                                          
016200     PERFORM IMS-GET-MSG                                                  
016300     IF SEGMENT-FINNS                                                     
016400       PERFORM A-INIT                                                     
016500       PERFORM B-KOLLA-NYCKLAR                                            
016600       IF NYCKLAR-OK                                                      
016700         IF MFS-UPDATE                                                    
016800           PERFORM G-KOLLA-INPUT                                          
016900           IF INDATA-OK                                                   
017000             PERFORM H-UPPDATERA                                          
017100           END-IF                                                         
017200         ELSE                                                             
017300           IF MFS-FIRST                                                   
017400             PERFORM C-FOERSTA-SIDA                                       
017500           ELSE                                                           
017600             PERFORM E-SAMMA-SIDA                                         
017700           END-IF                                                         
017800         END-IF                                                           
017900         IF ENTER-OCH-INDATA                                              
018000            CONTINUE                                                      
018100         ELSE                                                             
018200            IF INDATA-OK                                                  
018300               PERFORM F-LAES-VISA-INFO                                   
018400            END-IF                                                        
018500         END-IF                                                           
018600       END-IF                                                             
018700       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O13201 + 4                      
018800       PERFORM IMS-INSERT-MSG                                             
018900     END-IF                                                               
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     IF MSG-DUBBLA-TRANSKODER                                             
019800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I13201                 
019900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020100     ELSE                                                                 
020200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I13201                  
020300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020500     END-IF                                                               
020600                                                                          
020700     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
020800     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
020900     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
021000                                                                          
021100     MOVE LOW-VALUE         TO MSG-AREA                                   
021200     MOVE 'W1O132N1'        TO MFS-IDMOD                                  
021300     MOVE '1132'            TO MOD-IDTRANS                                
021400     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL MOD-TEMFSINF                  
021500                                                                          
021600     INSPECT MID-IDSPRAK-IN                                               
021700        CONVERTING LOWER-ALPHA        TO UPPER-ALPHA                      
021800                                                                          
021810*    LANDKOD SV BETYDER EL SALVADOR, INTE SVENSKA!                        
021820*    SÅ VI SÄTTER = SE VILKET MAN TROLIGEN MENAR                          
021830                                                                          
021900     IF MID-IDSPRAK-IN = 'SV'                                             
022000        MOVE 'SE'                     TO MID-IDSPRAK-IN                   
022100     END-IF                                                               
022200                                                                          
022300     IF MFS-IDTRANS = '1118'                                              
022400        MOVE MID-W1I13201  TO 1118-MID-W1I11801-CTX                       
022500        PERFORM AA-BEHANDLA-1118-TRANS                                    
022600     END-IF                                                               
022700                                                                          
022800     MOVE 'GB ' TO MED-IDSKYLT                                            
022900                                                                          
023700     .                                                                    
023800     EJECT                                                                
023900 AA-BEHANDLA-1118-TRANS SECTION.                                          
024000                                                                          
024100     MOVE 1118-MID-IDPSN-DOLT   TO MID-IDPSN-IN                           
024200     .                                                                    
024300     EJECT                                                                
024400 B-KOLLA-NYCKLAR SECTION.                                                 
024500                                                                          
024600     MOVE JA TO NYCKLAR-SW                                                
024601                                                                          
024610     MOVE MFS-RENSA-FAELT TO MOD-IDPSN-IN                                 
024620                             MOD-IDSPRAK-IN                               
024630                                                                          
024700                                                                          
024710     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024720     MOVE '001'             TO MSGI-KDCALL                                
024730     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024740     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024750     MOVE '1132'            TO MSGI-IDTRANS                               
024751                                                                          
024754     IF EGEN-MID OR HELP-MID                                              
024755       MOVE MID-IDSPRAK-IN TO MSGI-IDSPRAK-KEY                            
024756     ELSE                                                                 
024757       MOVE SPACE TO MFS-KDTRTYP                                          
024758       MOVE '7' TO MFS-IDPFK                                              
024759     END-IF                                                               
024762                                                                          
024763     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024770     MOVE MSGI-IDDC         TO WS-IDDC                                    
024780     MOVE MSGI-IDSPRAK-KEY  TO WS-IDSPRAK                                 
024781                               W-IDSPRAK                                  
024786                                                                          
024790* IDPSN                                                                   
025000     IF MID-IDPSN-IN = ALL '+'                                            
025100       MOVE MID-IDPSN-UT TO WS-IDPSN                                      
025200       INSPECT WS-IDPSN REPLACING LEADING SPACE BY ZERO                   
025300     ELSE                                                                 
025400       MOVE MID-IDPSN-IN TO WS-IDPSN                                      
025500       MOVE '7'         TO MFS-IDPFK                                      
025600       MOVE SPACE       TO MFS-KDTRTYP                                    
025700     END-IF                                                               
025800     IF WS-IDPSN NUMERIC AND WS-IDPSN > ZERO                              
025900       MOVE WS-IDPSN TO W-IDPSN                                           
026000     ELSE                                                                 
026100       MOVE NEJ TO NYCKLAR-SW                                             
026200     END-IF                                                               
026201                                                                          
026202* IDSPRAK                                                                 
026204                                                                          
026293     MOVE MSGI-IDSPRAK-KEY   TO LAND-IDLANDX2                             
026294     MOVE SPACE              TO LAND-IDLANDX3                             
026295     CALL WISOLAND USING LAND-WISOLAND                                    
026296     IF LAND-KDSVAR NOT = SPACE                                           
026300        MOVE NEJ TO NYCKLAR-SW                                            
026301     END-IF                                                               
026302                                                                          
026400     IF GODK-MID OR NYCKLAR-OK                                            
026500       MOVE WS-IDPSN TO MOD-IDPSN-UT                                      
026600       INSPECT MOD-IDPSN-UT REPLACING LEADING ZERO BY SPACE               
026610       MOVE WS-IDSPRAK      TO MOD-IDSPRAK-UT                             
026700     ELSE                                                                 
026800       MOVE MFS-RENSA-FAELT TO MOD-IDPSN-UT                               
026810                               MOD-IDSPRAK-UT                             
026900     END-IF                                                               
027000                                                                          
027100     IF NYCKLAR-FEL                                                       
027200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027300       CALL WMEDKONV USING MED-WMEDAREA                                   
027400       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
027500       PERFORM MFS-RENSA-FAELT-IN                                         
027600       PERFORM MFS-RENSA-FAELT-UT                                         
027700     END-IF                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 C-FOERSTA-SIDA SECTION.                                                  
028100                                                                          
028200     PERFORM MFS-RENSA-FAELT-IN                                           
028300     .                                                                    
028400     EJECT                                                                
028500 E-SAMMA-SIDA SECTION.                                                    
028600                                                                          
028700     IF EGEN-MID OR HELP-MID                                              
028800       IF MID-INPUT = ALL '+'                                             
028900         PERFORM MFS-RENSA-FAELT-IN                                       
029000       ELSE                                                               
029100         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
029200         CALL WMEDKONV USING MED-WMEDAREA                                 
029300         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
029400         PERFORM EA-MID-INDATA-TILL-MOD                                   
029500         MOVE JA TO ENTER-OCH-INDATA-SW                                   
029600       END-IF                                                             
029700     ELSE                                                                 
029800       PERFORM MFS-RENSA-FAELT-IN                                         
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 EA-MID-INDATA-TILL-MOD SECTION.                                          
030300                                                                          
030400*¤¤¤ DGR                                                                  
030500                                                                          
030600     MOVE +1 TO IX                                                        
030700     PERFORM UNTIL IX > 3                                                 
030800        IF MID-BEPSN-DGR(IX) NOT = ALL '+'                                
030900           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEPSN-DGR-ATTR(IX)           
031000        ELSE                                                              
031100           MOVE MFS-FORMATETS-ATTR    TO MOD-BEPSN-DGR-ATTR(IX)           
031200        END-IF                                                            
031300        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-DGR(IX)                
031400        ADD +1 TO IX                                                      
031500     END-PERFORM                                                          
031600                                                                          
031700*¤¤¤ IMDG                                                                 
031800                                                                          
031900     MOVE +1 TO IX                                                        
032000     PERFORM UNTIL IX > 3                                                 
032100        IF MID-BEPSN-IMDG(IX) NOT = ALL '+'                               
032200           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEPSN-IMDG-ATTR(IX)          
032300        ELSE                                                              
032400           MOVE MFS-RENSA-FAELT       TO MOD-BEPSN-IMDG(IX)               
032500        END-IF                                                            
032600        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-IMDG(IX)               
032700        ADD +1 TO IX                                                      
032800     END-PERFORM                                                          
032900                                                                          
033000*¤¤¤ IMDG-SF                                                              
033100                                                                          
033200     MOVE +1 TO IX                                                        
033300     PERFORM UNTIL IX > 3                                                 
033400        IF MID-BEPSN-IMDG-SF(IX) NOT = ALL '+'                            
033500           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
033600                         MOD-BEPSN-IMDG-SF-ATTR(IX)                       
033700        ELSE                                                              
033800           MOVE MFS-FORMATETS-ATTR    TO                                  
033900                         MOD-BEPSN-IMDG-SF-ATTR(IX)                       
034000        END-IF                                                            
034100        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-IMDG-SF(IX)            
034200        ADD +1 TO IX                                                      
034300     END-PERFORM                                                          
034400                                                                          
034500*¤¤¤ ADR                                                                  
034600                                                                          
034700     MOVE +1 TO IX                                                        
034800     PERFORM UNTIL IX > 3                                                 
034900        IF MID-BEPSN-ADR(IX) NOT = ALL '+'                                
035000           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEPSN-ADR-ATTR(IX)           
035100        ELSE                                                              
035200           MOVE MFS-FORMATETS-ATTR    TO MOD-BEPSN-ADR-ATTR(IX)           
035300        END-IF                                                            
035400        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-ADR(IX)                
035500        ADD +1 TO IX                                                      
035600     END-PERFORM                                                          
035700                                                                          
035800*¤¤¤ NOT                                                                  
035900                                                                          
036000     MOVE +1 TO IX                                                        
036100     PERFORM UNTIL IX > 3                                                 
036200        IF MID-TEPSNNOT(IX) NOT = ALL '+'                                 
036300           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEPSNNOT-ATTR(IX)            
036400        ELSE                                                              
036500           MOVE MFS-FORMATETS-ATTR    TO MOD-TEPSNNOT-ATTR(IX)            
036600        END-IF                                                            
036700        MOVE MFS-ROER-EJ-FAELT        TO MOD-TEPSNNOT(IX)                 
036800        ADD +1 TO IX                                                      
036900     END-PERFORM                                                          
037000     .                                                                    
037100     EJECT                                                                
037200 F-LAES-VISA-INFO SECTION.                                                
037300                                                                          
037400     PERFORM FA-LAES-GRUNDDATA                                            
037500                                                                          
037600     IF SEGMENT-SAKNAS                                                    
037700        MOVE FEL-1      TO MOD-TEMFSFEL                                   
037800        PERFORM MFS-RENSA-FAELT-UT                                        
037900     ELSE                                                                 
038000        PERFORM FB-VISA-BILD                                              
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 FA-LAES-GRUNDDATA SECTION.                                               
038500                                                                          
038600     PERFORM IMS-GET-1165                                                 
038700     .                                                                    
038800     EJECT                                                                
038900 FB-VISA-BILD SECTION.                                                    
039000                                                                          
039100     PERFORM FBA-LAS-1168-DGR                                             
039200     PERFORM FBB-LAS-1168-IMDG                                            
039300     PERFORM FBC-LAS-1168-IMDG-SF                                         
039400     PERFORM FBD-LAS-1168-ADR                                             
039500     PERFORM FBE-LAS-1166-NOT                                             
039600     .                                                                    
039700     EJECT                                                                
039800 FBA-LAS-1168-DGR SECTION.                                                
039900                                                                          
040000     MOVE 01 TO W-KDFGTRP                                                 
040100     PERFORM IMS-GET-1168                                                 
040200     IF SEGMENT-FINNS                                                     
040300                                                                          
040400        MOVE +1 TO IX                                                     
040500        PERFORM UNTIL IX > 3                                              
040600           IF 1168-BEPSN(IX) = SPACE                                      
040700              MOVE MFS-RENSA-FAELT    TO MOD-BEPSN-DGR(IX)                
040800           ELSE                                                           
040900              MOVE 1168-BEPSN(IX)     TO MOD-BEPSN-DGR(IX)                
041000           END-IF                                                         
041100           ADD +1 TO IX                                                   
041200        END-PERFORM                                                       
041300     ELSE                                                                 
041400        MOVE +1 TO IX                                                     
041500        PERFORM UNTIL IX > 3                                              
041600           MOVE MFS-RENSA-FAELT       TO MOD-BEPSN-DGR(IX)                
041700           ADD +1 TO IX                                                   
041800        END-PERFORM                                                       
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 FBB-LAS-1168-IMDG SECTION.                                               
042300                                                                          
042400     MOVE 02 TO W-KDFGTRP                                                 
042500     PERFORM IMS-GET-1168                                                 
042600     IF SEGMENT-FINNS                                                     
042700                                                                          
042800        MOVE +1 TO IX                                                     
042900        PERFORM UNTIL IX > 3                                              
043000           IF 1168-BEPSN(IX) = SPACE                                      
043100              MOVE MFS-RENSA-FAELT       TO MOD-BEPSN-IMDG(IX)            
043200           ELSE                                                           
043300              MOVE 1168-BEPSN(IX)        TO MOD-BEPSN-IMDG(IX)            
043400           END-IF                                                         
043500           ADD +1 TO IX                                                   
043600        END-PERFORM                                                       
043700     ELSE                                                                 
043800        MOVE +1 TO IX                                                     
043900        PERFORM UNTIL IX > 3                                              
044000           MOVE MFS-RENSA-FAELT       TO MOD-BEPSN-IMDG(IX)               
044100           ADD +1 TO IX                                                   
044200        END-PERFORM                                                       
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 FBC-LAS-1168-IMDG-SF SECTION.                                            
044700                                                                          
044800     MOVE 03 TO W-KDFGTRP                                                 
044900     PERFORM IMS-GET-1168                                                 
045000     IF SEGMENT-FINNS                                                     
045100        MOVE +1 TO IX                                                     
045200        PERFORM UNTIL IX > 3                                              
045300           IF 1168-BEPSN(IX) = SPACE                                      
045400              MOVE MFS-RENSA-FAELT    TO MOD-BEPSN-IMDG-SF(IX)            
045500           ELSE                                                           
045600              MOVE 1168-BEPSN(IX)     TO MOD-BEPSN-IMDG-SF(IX)            
045700           END-IF                                                         
045800           ADD +1 TO IX                                                   
045900        END-PERFORM                                                       
046000     ELSE                                                                 
046100        MOVE +1 TO IX                                                     
046200        PERFORM UNTIL IX > 3                                              
046300           MOVE MFS-RENSA-FAELT       TO MOD-BEPSN-IMDG-SF(IX)            
046400           ADD +1 TO IX                                                   
046500        END-PERFORM                                                       
046600     END-IF                                                               
046700     .                                                                    
046800     EJECT                                                                
046900 FBD-LAS-1168-ADR SECTION.                                                
047000                                                                          
047100     MOVE 04 TO W-KDFGTRP                                                 
047200     PERFORM IMS-GET-1168                                                 
047300     IF SEGMENT-FINNS                                                     
047400        MOVE +1 TO IX                                                     
047500        PERFORM UNTIL IX > 3                                              
047600           IF 1168-BEPSN(IX) = SPACE                                      
047700              MOVE MFS-RENSA-FAELT    TO MOD-BEPSN-ADR(IX)                
047800           ELSE                                                           
047900              MOVE 1168-BEPSN(IX)     TO MOD-BEPSN-ADR(IX)                
048000           END-IF                                                         
048100           ADD +1 TO IX                                                   
048200        END-PERFORM                                                       
048300     ELSE                                                                 
048400        MOVE +1 TO IX                                                     
048500        PERFORM UNTIL IX > 3                                              
048600           MOVE MFS-RENSA-FAELT       TO MOD-BEPSN-ADR(IX)                
048700           ADD +1 TO IX                                                   
048800        END-PERFORM                                                       
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 FBE-LAS-1166-NOT SECTION.                                                
049300                                                                          
049400     PERFORM IMS-GET-1166                                                 
049500     IF SEGMENT-FINNS                                                     
049600        MOVE +1 TO IX                                                     
049700        PERFORM UNTIL IX > 3                                              
049800           IF 1166-TEPSNNOT(IX) = SPACE                                   
049900              MOVE MFS-RENSA-FAELT      TO MOD-TEPSNNOT(IX)               
050000           ELSE                                                           
050100              MOVE 1166-TEPSNNOT(IX)    TO MOD-TEPSNNOT(IX)               
050200           END-IF                                                         
050300           ADD +1 TO IX                                                   
050400        END-PERFORM                                                       
050500     ELSE                                                                 
050600        MOVE +1 TO IX                                                     
050700        PERFORM UNTIL IX > 3                                              
050800           MOVE MFS-RENSA-FAELT         TO MOD-TEPSNNOT(IX)               
050900           ADD +1 TO IX                                                   
051000        END-PERFORM                                                       
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 G-KOLLA-INPUT SECTION.                                                   
051500                                                                          
051600     MOVE JA  TO INDATA-SW                                                
051700     IF MID-INPUT = ALL '+'                                               
051800        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
051900        CALL WMEDKONV USING MED-WMEDAREA                                  
052000        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
052100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
052200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
052300        MOVE NEJ TO INDATA-SW                                             
052400     ELSE                                                                 
052500        IF CDC-SE                                                         
052510          IF WS-IDPSN < 100                                               
052511            MOVE +1 TO IX                                                 
052512            PERFORM UNTIL IX > 3 OR INDATA-FEL                            
052520              IF MID-BEPSN-DGR(IX)(71:5) NOT = SPACE                      
052521              AND MID-BEPSN-DGR(IX)(71:5) NOT = ALL '+'                   
052530                MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                   
052540                CALL WMEDKONV USING MED-WMEDAREA                          
052550                MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                         
052571                MOVE 'ONLY 70 CHARACTERS FOR PSN < 100'                   
052572                     TO MOD-TEMFSINF                                      
052580                MOVE NEJ TO INDATA-SW                                     
052581                MOVE MFS-ALPHA-FIELD-WRONG                                
052582                     TO MOD-BEPSN-DGR-ATTR(IX)                            
052583                PERFORM MFS-ROER-EJ-FAELT-IN                              
052584                PERFORM MFS-ROER-EJ-FAELT-UT                              
052600              END-IF                                                      
052601              ADD +1 TO IX                                                
052610            END-PERFORM                                                   
052700          END-IF                                                          
052800        ELSE                                                              
052900           IF WS-IDPSN < 100                                              
053000              MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                     
053100              CALL WMEDKONV USING MED-WMEDAREA                            
053200              MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                           
053300              PERFORM MFS-ROER-EJ-FAELT-IN                                
053400              PERFORM MFS-ROER-EJ-FAELT-UT                                
053500              MOVE NEJ TO INDATA-SW                                       
053600           END-IF                                                         
053700        END-IF                                                            
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 H-UPPDATERA SECTION.                                                     
054200                                                                          
054300     PERFORM IMS-GET-1165                                                 
054400     IF SEGMENT-FINNS                                                     
054500        CONTINUE                                                          
054600     ELSE                                                                 
054700        MOVE '1165'                   TO 1165-IDHTYP                      
054800        MOVE WS-IDPSN                 TO 1165-IDPSN W-IDPSN               
054810        MOVE WS-IDSPRAK               TO 1165-IDSPRAK                     
054820                                         W-IDSPRAK                        
054900        MOVE LOW-VALUE                TO 1165-LOWVALUE                    
055000        PERFORM IMS-ISRT-1165                                             
055100     END-IF                                                               
055200                                                                          
055300     PERFORM HA-UPPDATERA-IN-GRUPPER                                      
055400     MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                                 
055500     CALL WMEDKONV USING MED-WMEDAREA                                     
055600     MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                    
055610     IF WS-IDSPRAK = 'SE'                                                 
055620       MOVE 'OBS! UPPD. ALLA SPRÅK!' TO MOD-TEMFSFEL (17:22)              
055630     END-IF                                                               
055700     PERFORM MFS-RENSA-FAELT-IN                                           
055800     .                                                                    
055900     EJECT                                                                
056000 HA-UPPDATERA-IN-GRUPPER SECTION.                                         
056100                                                                          
056200     PERFORM IMS-GET-1165                                                 
056300                                                                          
056400     IF MID-GRUPP-DGR NOT = ALL '+'                                       
056500        PERFORM HAA-UPPDATERA-DGR                                         
056600     END-IF                                                               
056700                                                                          
056800     IF MID-GRUPP-IMDG NOT = ALL '+'                                      
056900        PERFORM HAB-UPPDATERA-IMDG                                        
057000     END-IF                                                               
057100                                                                          
057200     IF MID-GRUPP-IMDG-SF NOT = ALL '+'                                   
057300        PERFORM HAC-UPPDATERA-IMDG-SF                                     
057400     END-IF                                                               
057500                                                                          
057600     IF MID-GRUPP-ADR NOT = ALL '+'                                       
057700        PERFORM HAD-UPPDATERA-ADR                                         
057800     END-IF                                                               
057900                                                                          
058000     IF MID-GRUPP-NOT NOT = ALL '+'                                       
058100        PERFORM HAE-UPPDATERA-NOT                                         
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500 HAA-UPPDATERA-DGR SECTION.                                               
058600                                                                          
058700     MOVE 01 TO W-KDFGTRP                                                 
058800     PERFORM IMS-GET-1168                                                 
058900     IF SEGMENT-FINNS                                                     
059000        PERFORM HAA1-FLYTTA-DGR-REPL                                      
059100        PERFORM HX-KOLLA-DLET                                             
059200        IF DLET-KDFGTRP-SW = JA                                           
059300           PERFORM IMS-DLET-1168                                          
059400        ELSE                                                              
059500           PERFORM IMS-REPL-1168                                          
059600        END-IF                                                            
059700     ELSE                                                                 
059800        PERFORM HAA2-FLYTTA-DGR-ISRT                                      
059900        PERFORM IMS-ISRT-1168                                             
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300 HAA1-FLYTTA-DGR-REPL SECTION.                                            
060400                                                                          
060500     MOVE +1 TO IX                                                        
060600     PERFORM UNTIL IX > 3                                                 
060700       IF MID-BEPSN-DGR(IX) NOT = ALL '+'                                 
060800          MOVE MID-BEPSN-DGR(IX)       TO 1168-BEPSN(IX)                  
060900          MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-BEPSN-DGR-ATTR(IX)          
061000       ELSE                                                               
061100          MOVE MFS-FORMATETS-ATTR      TO MOD-BEPSN-DGR-ATTR(IX)          
061200       END-IF                                                             
061300       ADD +1 TO IX                                                       
061400     END-PERFORM                                                          
061500     .                                                                    
061600     EJECT                                                                
061700 HAA2-FLYTTA-DGR-ISRT SECTION.                                            
061800                                                                          
061900     MOVE 01                          TO 1168-KDFGTRP                     
062000                                                                          
062100     MOVE +1 TO IX                                                        
062200     PERFORM UNTIL IX > 3                                                 
062300       IF MID-BEPSN-DGR(IX) NOT = ALL '+'                                 
062400          MOVE MID-BEPSN-DGR(IX)       TO 1168-BEPSN(IX)                  
062500          MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-BEPSN-DGR-ATTR(IX)          
062600       ELSE                                                               
062700          MOVE SPACE                   TO 1168-BEPSN(IX)                  
062800          MOVE MFS-FORMATETS-ATTR      TO MOD-BEPSN-DGR-ATTR(IX)          
062900       END-IF                                                             
063000       ADD +1 TO IX                                                       
063100     END-PERFORM                                                          
063200     .                                                                    
063300     EJECT                                                                
063400 HAB-UPPDATERA-IMDG SECTION.                                              
063500                                                                          
063600     MOVE 02 TO W-KDFGTRP                                                 
063700     PERFORM IMS-GET-1168                                                 
063800     IF SEGMENT-FINNS                                                     
063900        PERFORM HAB1-FLYTTA-IMDG-REPL                                     
064000        PERFORM HX-KOLLA-DLET                                             
064100        IF DLET-KDFGTRP-SW = JA                                           
064200           PERFORM IMS-DLET-1168                                          
064300        ELSE                                                              
064400           PERFORM IMS-REPL-1168                                          
064500        END-IF                                                            
064600     ELSE                                                                 
064700        PERFORM HAB2-FLYTTA-IMDG-ISRT                                     
064800        PERFORM IMS-ISRT-1168                                             
064900     END-IF                                                               
065000     .                                                                    
065100     EJECT                                                                
065200 HAB1-FLYTTA-IMDG-REPL SECTION.                                           
065300                                                                          
065400     MOVE +1 TO IX                                                        
065500     PERFORM UNTIL IX > 3                                                 
065600       IF MID-BEPSN-IMDG(IX) NOT = ALL '+'                                
065700          MOVE MID-BEPSN-IMDG(IX)     TO 1168-BEPSN(IX)                   
065800          MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-BEPSN-IMDG-ATTR(IX)          
065900       ELSE                                                               
066000          MOVE MFS-FORMATETS-ATTR     TO MOD-BEPSN-IMDG-ATTR(IX)          
066100       END-IF                                                             
066200       ADD +1 TO IX                                                       
066300     END-PERFORM                                                          
066400     .                                                                    
066500     EJECT                                                                
066600 HAB2-FLYTTA-IMDG-ISRT SECTION.                                           
066700                                                                          
066800     MOVE 02                          TO 1168-KDFGTRP                     
066900                                                                          
067000     MOVE +1 TO IX                                                        
067100     PERFORM UNTIL IX > 3                                                 
067200       IF MID-BEPSN-IMDG(IX) NOT = ALL '+'                                
067300          MOVE MID-BEPSN-IMDG(IX)     TO 1168-BEPSN(IX)                   
067400          MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-BEPSN-IMDG-ATTR(IX)          
067500       ELSE                                                               
067600          MOVE SPACE                  TO 1168-BEPSN(IX)                   
067700          MOVE MFS-FORMATETS-ATTR     TO MOD-BEPSN-IMDG-ATTR(IX)          
067800       END-IF                                                             
067900       ADD +1 TO IX                                                       
068000     END-PERFORM                                                          
068100     .                                                                    
068200     EJECT                                                                
068300 HAC-UPPDATERA-IMDG-SF SECTION.                                           
068400                                                                          
068500     MOVE 03 TO W-KDFGTRP                                                 
068600     PERFORM IMS-GET-1168                                                 
068700     IF SEGMENT-FINNS                                                     
068800        PERFORM HAC1-FLYTTA-IMDG-SF-REPL                                  
068900        PERFORM HX-KOLLA-DLET                                             
069000        IF DLET-KDFGTRP-SW = JA                                           
069100           PERFORM IMS-DLET-1168                                          
069200        ELSE                                                              
069300           PERFORM IMS-REPL-1168                                          
069400        END-IF                                                            
069500     ELSE                                                                 
069600        PERFORM HAC2-FLYTTA-IMDG-SF-ISRT                                  
069700        PERFORM IMS-ISRT-1168                                             
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 HAC1-FLYTTA-IMDG-SF-REPL SECTION.                                        
070200                                                                          
070300     MOVE +1 TO IX                                                        
070400     PERFORM UNTIL IX > 3                                                 
070500       IF MID-BEPSN-IMDG-SF(IX) NOT = ALL '+'                             
070600          MOVE MID-BEPSN-IMDG-SF(IX)  TO 1168-BEPSN(IX)                   
070700          MOVE MFS-ADD-LYS-UPP-FAELT  TO                                  
070800                             MOD-BEPSN-IMDG-SF-ATTR(IX)                   
070900       ELSE                                                               
071000          MOVE MFS-FORMATETS-ATTR     TO                                  
071100                             MOD-BEPSN-IMDG-SF-ATTR(IX)                   
071200       END-IF                                                             
071300       ADD +1 TO IX                                                       
071400     END-PERFORM                                                          
071500     .                                                                    
071600     EJECT                                                                
071700 HAC2-FLYTTA-IMDG-SF-ISRT SECTION.                                        
071800                                                                          
071900     MOVE 03                          TO 1168-KDFGTRP                     
072000                                                                          
072100     MOVE +1 TO IX                                                        
072200     PERFORM UNTIL IX > 3                                                 
072300       IF MID-BEPSN-IMDG-SF(IX) NOT = ALL '+'                             
072400          MOVE MID-BEPSN-IMDG-SF(IX)  TO 1168-BEPSN(IX)                   
072500          MOVE MFS-ADD-LYS-UPP-FAELT  TO                                  
072600                            MOD-BEPSN-IMDG-SF-ATTR(IX)                    
072700       ELSE                                                               
072800          MOVE SPACE                  TO 1168-BEPSN(IX)                   
072900          MOVE MFS-FORMATETS-ATTR     TO                                  
073000                            MOD-BEPSN-IMDG-SF-ATTR(IX)                    
073100       END-IF                                                             
073200       ADD +1 TO IX                                                       
073300     END-PERFORM                                                          
073400     .                                                                    
073500     EJECT                                                                
073600 HAD-UPPDATERA-ADR SECTION.                                               
073700                                                                          
073800     MOVE 04 TO W-KDFGTRP                                                 
073900     PERFORM IMS-GET-1168                                                 
074000     IF SEGMENT-FINNS                                                     
074100        PERFORM HAD1-FLYTTA-ADR-REPL                                      
074200        PERFORM HX-KOLLA-DLET                                             
074300        IF DLET-KDFGTRP-SW = JA                                           
074400           PERFORM IMS-DLET-1168                                          
074500        ELSE                                                              
074600           PERFORM IMS-REPL-1168                                          
074700        END-IF                                                            
074800     ELSE                                                                 
074900        PERFORM HAD2-FLYTTA-ADR-ISRT                                      
075000        PERFORM IMS-ISRT-1168                                             
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 HAD1-FLYTTA-ADR-REPL SECTION.                                            
075500                                                                          
075600     MOVE +1 TO IX                                                        
075700     PERFORM UNTIL IX > 3                                                 
075800       IF MID-BEPSN-ADR(IX) NOT = ALL '+'                                 
075900          MOVE MID-BEPSN-ADR(IX)      TO 1168-BEPSN(IX)                   
076000          MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-BEPSN-ADR-ATTR(IX)           
076100       ELSE                                                               
076200          MOVE MFS-FORMATETS-ATTR     TO MOD-BEPSN-ADR-ATTR(IX)           
076300       END-IF                                                             
076400       ADD +1 TO IX                                                       
076500     END-PERFORM                                                          
076600     .                                                                    
076700     EJECT                                                                
076800 HAD2-FLYTTA-ADR-ISRT SECTION.                                            
076900                                                                          
077000     MOVE 04                          TO 1168-KDFGTRP                     
077100                                                                          
077200     MOVE +1 TO IX                                                        
077300     PERFORM UNTIL IX > 3                                                 
077400       IF MID-BEPSN-ADR(IX) NOT = ALL '+'                                 
077500          MOVE MID-BEPSN-ADR(IX)      TO 1168-BEPSN(IX)                   
077600          MOVE MFS-ADD-LYS-UPP-FAELT  TO                                  
077700                            MOD-BEPSN-ADR-ATTR(IX)                        
077800       ELSE                                                               
077900          MOVE SPACE                  TO 1168-BEPSN(IX)                   
078000          MOVE MFS-FORMATETS-ATTR     TO                                  
078100                            MOD-BEPSN-ADR-ATTR(IX)                        
078200       END-IF                                                             
078300       ADD +1 TO IX                                                       
078400     END-PERFORM                                                          
078500     .                                                                    
078600     EJECT                                                                
078700 HAE-UPPDATERA-NOT  SECTION.                                              
078800                                                                          
078900     PERFORM IMS-GET-1166                                                 
079000     IF SEGMENT-FINNS                                                     
079100        PERFORM HAE1-FLYTTA-NOT-REPL                                      
079200        PERFORM HZ-KOLLA-DLET-NOT                                         
079300        IF DLET-1166-SW = JA                                              
079400           PERFORM IMS-DLET-1166                                          
079500        ELSE                                                              
079600           PERFORM IMS-REPL-1166                                          
079700        END-IF                                                            
079800     ELSE                                                                 
079900        PERFORM HAE2-FLYTTA-NOT-ISRT                                      
080000        PERFORM IMS-ISRT-1166                                             
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400 HAE1-FLYTTA-NOT-REPL SECTION.                                            
080500                                                                          
080600     MOVE +1 TO IX                                                        
080700     PERFORM UNTIL IX > 3                                                 
080800        IF MID-TEPSNNOT(IX) NOT = ALL '+'                                 
080900           MOVE MID-TEPSNNOT(IX)     TO 1166-TEPSNNOT(IX)                 
081000           MOVE MFS-ADD-LYS-UPP-FAELT  TO                                 
081100                               MOD-TEPSNNOT-ATTR(IX)                      
081200        ELSE                                                              
081300           MOVE MFS-FORMATETS-ATTR TO MOD-TEPSNNOT-ATTR(IX)               
081400        END-IF                                                            
081500        ADD +1 TO IX                                                      
081600     END-PERFORM                                                          
081700     .                                                                    
081800     EJECT                                                                
081900 HAE2-FLYTTA-NOT-ISRT SECTION.                                            
082000                                                                          
082100     MOVE '1'                        TO 1166-KDSEGKEY                     
082200                                                                          
082300     MOVE +1 TO IX                                                        
082400     PERFORM UNTIL IX > 3                                                 
082500        IF MID-TEPSNNOT(IX) NOT = ALL '+'                                 
082600           MOVE MID-TEPSNNOT(IX)     TO 1166-TEPSNNOT(IX)                 
082700           MOVE MFS-ADD-LYS-UPP-FAELT  TO                                 
082800                               MOD-TEPSNNOT-ATTR(IX)                      
082900        ELSE                                                              
083000           MOVE SPACE              TO 1166-TEPSNNOT(IX)                   
083100           MOVE MFS-FORMATETS-ATTR TO MOD-TEPSNNOT-ATTR(IX)               
083200        END-IF                                                            
083300        ADD +1 TO IX                                                      
083400     END-PERFORM                                                          
083500     .                                                                    
083600     EJECT                                                                
083700 HX-KOLLA-DLET SECTION.                                                   
083800                                                                          
083900     MOVE NEJ TO DLET-KDFGTRP-SW                                          
084000     IF 1168-BEPSN(1) = SPACE AND                                         
084100        1168-BEPSN(2) = SPACE AND                                         
084200        1168-BEPSN(3) = SPACE                                             
084300        MOVE JA TO DLET-KDFGTRP-SW                                        
084400     END-IF                                                               
084500      .                                                                   
084600      EJECT                                                               
084700 HZ-KOLLA-DLET-NOT SECTION.                                               
084800                                                                          
084900     MOVE NEJ TO DLET-1166-SW                                             
085000     IF 1166-TEPSNNOT(1) = SPACE AND                                      
085100        1166-TEPSNNOT(2) = SPACE AND                                      
085200        1166-TEPSNNOT(3) = SPACE                                          
085300        MOVE JA TO DLET-1166-SW                                           
085400     END-IF                                                               
085500      .                                                                   
085600      EJECT                                                               
085700 MFS-RENSA-FAELT-UT SECTION.                                              
085800                                                                          
085900     MOVE +1 TO IX                                                        
086000     PERFORM UNTIL IX > 3                                                 
086100        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-DGR(IX)                
086200        ADD +1 TO IX                                                      
086300     END-PERFORM                                                          
086400                                                                          
086500     MOVE +1 TO IX                                                        
086600     PERFORM UNTIL IX > 3                                                 
086700        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-IMDG(IX)               
086800        ADD +1 TO IX                                                      
086900     END-PERFORM                                                          
087000                                                                          
087100     MOVE +1 TO IX                                                        
087200     PERFORM UNTIL IX > 3                                                 
087300        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-IMDG-SF(IX)            
087400        ADD +1 TO IX                                                      
087500     END-PERFORM                                                          
087600                                                                          
087700     MOVE +1 TO IX                                                        
087800     PERFORM UNTIL IX > 3                                                 
087900        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-ADR(IX)                
088000        ADD +1 TO IX                                                      
088100     END-PERFORM                                                          
088200                                                                          
088300     MOVE +1 TO IX                                                        
088400     PERFORM UNTIL IX > 3                                                 
088500        MOVE MFS-RENSA-FAELT          TO MOD-TEPSNNOT(IX)                 
088600        ADD +1 TO IX                                                      
088700     END-PERFORM                                                          
088800     .                                                                    
088900     EJECT                                                                
089000 MFS-RENSA-FAELT-IN SECTION.                                              
089100                                                                          
089200     MOVE +1 TO IX                                                        
089300     PERFORM UNTIL IX > 3                                                 
089400        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-DGR(IX)                
089500        ADD +1 TO IX                                                      
089600     END-PERFORM                                                          
089700                                                                          
089800     MOVE +1 TO IX                                                        
089900     PERFORM UNTIL IX > 3                                                 
090000        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-IMDG(IX)               
090100        ADD +1 TO IX                                                      
090200     END-PERFORM                                                          
090300                                                                          
090400     MOVE +1 TO IX                                                        
090500     PERFORM UNTIL IX > 3                                                 
090600        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-IMDG-SF(IX)            
090700        ADD +1 TO IX                                                      
090800     END-PERFORM                                                          
090900                                                                          
091000     MOVE +1 TO IX                                                        
091100     PERFORM UNTIL IX > 3                                                 
091200        MOVE MFS-RENSA-FAELT          TO MOD-BEPSN-ADR(IX)                
091300        ADD +1 TO IX                                                      
091400     END-PERFORM                                                          
091500                                                                          
091600     MOVE +1 TO IX                                                        
091700     PERFORM UNTIL IX > 3                                                 
091800        MOVE MFS-RENSA-FAELT          TO MOD-TEPSNNOT(IX)                 
091900        ADD +1 TO IX                                                      
092000     END-PERFORM                                                          
092100     .                                                                    
092200     EJECT                                                                
092300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
092400                                                                          
092500     MOVE +1 TO IX                                                        
092600     PERFORM UNTIL IX > 3                                                 
092700        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-DGR(IX)                
092800        ADD +1 TO IX                                                      
092900     END-PERFORM                                                          
093000                                                                          
093100     MOVE +1 TO IX                                                        
093200     PERFORM UNTIL IX > 3                                                 
093300        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-IMDG(IX)               
093400        ADD +1 TO IX                                                      
093500     END-PERFORM                                                          
093600                                                                          
093700     MOVE +1 TO IX                                                        
093800     PERFORM UNTIL IX > 3                                                 
093900        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-IMDG-SF(IX)            
094000        ADD +1 TO IX                                                      
094100     END-PERFORM                                                          
094200                                                                          
094300     MOVE +1 TO IX                                                        
094400     PERFORM UNTIL IX > 3                                                 
094500        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-ADR(IX)                
094600        ADD +1 TO IX                                                      
094700     END-PERFORM                                                          
094800                                                                          
094900     MOVE +1 TO IX                                                        
095000     PERFORM UNTIL IX > 3                                                 
095100        MOVE MFS-ROER-EJ-FAELT        TO MOD-TEPSNNOT(IX)                 
095200        ADD +1 TO IX                                                      
095300     END-PERFORM                                                          
095400     .                                                                    
095500     EJECT                                                                
095600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
095700                                                                          
095800     MOVE +1 TO IX                                                        
095900     PERFORM UNTIL IX > 3                                                 
096000        MOVE MFS-ROER-EJ-FAELT       TO MOD-BEPSN-DGR(IX)                 
096100        ADD +1 TO IX                                                      
096200     END-PERFORM                                                          
096300                                                                          
096400     MOVE +1 TO IX                                                        
096500     PERFORM UNTIL IX > 3                                                 
096600        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-IMDG(IX)               
096700        ADD +1 TO IX                                                      
096800     END-PERFORM                                                          
096900                                                                          
097000     MOVE +1 TO IX                                                        
097100     PERFORM UNTIL IX > 3                                                 
097200        MOVE MFS-ROER-EJ-FAELT        TO MOD-BEPSN-IMDG-SF(IX)            
097300        ADD +1 TO IX                                                      
097400     END-PERFORM                                                          
097500                                                                          
097600     MOVE +1 TO IX                                                        
097700     PERFORM UNTIL IX > 3                                                 
097800        MOVE MFS-ROER-EJ-FAELT TO     MOD-BEPSN-ADR(IX)                   
097900        ADD +1 TO IX                                                      
098000     END-PERFORM                                                          
098100                                                                          
098200     MOVE +1 TO IX                                                        
098300     PERFORM UNTIL IX > 3                                                 
098400        MOVE MFS-ROER-EJ-FAELT TO     MOD-TEPSNNOT(IX)                    
098500        ADD +1 TO IX                                                      
098600     END-PERFORM                                                          
098700     .                                                                    
098800     EJECT                                                                
098900* --- IMS SEKTIONER ---                                                   
099000     SKIP3                                                                
099100 IMS-GET-MSG SECTION.                                                     
099200     MOVE '  QC' TO GODK-STATUSKODER                                      
099300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
099400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099500     PERFORM IMS-STATUSKONTROLL                                           
099600     .                                                                    
099700     SKIP3                                                                
099800 IMS-INSERT-MSG SECTION.                                                  
099900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
100000     MOVE SPACE TO GODK-STATUSKODER                                       
100100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
100200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     EJECT                                                                
100600 IMS-GET-1165 SECTION.                                                    
100620                                                                          
100700     STRING 'WL116501(WDGXKEY  =' W-1165KEY-X ')'                         
100800          DELIMITED BY SIZE INTO SSA1                                     
100900     MOVE '  GE' TO GODK-STATUSKODER                                      
101000     CALL CBLTDLI USING GU 1165-PCB DLI-IO-AREA SSA1                      
101100     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     SKIP3                                                                
101500 IMS-ISRT-1165 SECTION.                                                   
101600     MOVE 'WL116501 ' TO SSA1                                             
101700     MOVE '  ' TO GODK-STATUSKODER                                        
101800     CALL CBLTDLI USING ISRT 1165-PCB DLI-IO-AREA SSA1                    
101900     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
102000     PERFORM IMS-STATUSKONTROLL                                           
102100     .                                                                    
102200     EJECT                                                                
102300 IMS-GET-1166 SECTION.                                                    
102400     MOVE 'WL116511*F' TO SSA1                                            
102500     MOVE '  GE' TO GODK-STATUSKODER                                      
102600     CALL CBLTDLI USING GHNP 1165-PCB DLI-IO-AREA SSA1                    
102700     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
102800     PERFORM IMS-STATUSKONTROLL                                           
102900     .                                                                    
103000     SKIP3                                                                
103100 IMS-ISRT-1166 SECTION.                                                   
103200     STRING 'WL116501(WDGXKEY  =' W-1165KEY-X ')'                         
103300          DELIMITED BY SIZE INTO SSA1                                     
103400     STRING 'WL116511 '                                                   
103500          DELIMITED BY SIZE INTO SSA2                                     
103600     MOVE '  ' TO GODK-STATUSKODER                                        
103700     CALL CBLTDLI USING ISRT 1165-PCB DLI-IO-AREA SSA1 SSA2               
103800     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
103900     PERFORM IMS-STATUSKONTROLL                                           
104000     .                                                                    
104100     SKIP3                                                                
104200 IMS-REPL-1166 SECTION.                                                   
104300     MOVE '  ' TO GODK-STATUSKODER                                        
104400     CALL CBLTDLI USING REPL 1165-PCB DLI-IO-AREA                         
104500     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
104600     PERFORM IMS-STATUSKONTROLL                                           
104700     .                                                                    
104800     EJECT                                                                
104900 IMS-DLET-1166 SECTION.                                                   
105000     MOVE '  ' TO GODK-STATUSKODER                                        
105100     CALL CBLTDLI USING DLET 1165-PCB DLI-IO-AREA                         
105200     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
105300     PERFORM IMS-STATUSKONTROLL                                           
105400     .                                                                    
105500     SKIP3                                                                
105600 IMS-GET-1168 SECTION.                                                    
105700     STRING 'WL116512*F(KDFGTRP  =' W-1168KEY-X ')'                       
105800          DELIMITED BY SIZE INTO SSA1                                     
105900     MOVE '  GE' TO GODK-STATUSKODER                                      
106000     CALL CBLTDLI USING GHNP 1165-PCB DLI-IO-AREA SSA1                    
106100     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
106200     PERFORM IMS-STATUSKONTROLL                                           
106300     .                                                                    
106400     SKIP3                                                                
106500 IMS-ISRT-1168 SECTION.                                                   
106600     STRING 'WL116501(WDGXKEY  =' W-1165KEY-X ')'                         
106700          DELIMITED BY SIZE INTO SSA1                                     
106800     STRING 'WL116512 '                                                   
106900          DELIMITED BY SIZE INTO SSA2                                     
107000     MOVE '  ' TO GODK-STATUSKODER                                        
107100     CALL CBLTDLI USING ISRT 1165-PCB DLI-IO-AREA SSA1 SSA2               
107200     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500     SKIP3                                                                
107600 IMS-REPL-1168 SECTION.                                                   
107700     MOVE '  ' TO GODK-STATUSKODER                                        
107800     CALL CBLTDLI USING REPL 1165-PCB DLI-IO-AREA                         
107900     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     EJECT                                                                
108300 IMS-DLET-1168 SECTION.                                                   
108400     MOVE '  ' TO GODK-STATUSKODER                                        
108500     CALL CBLTDLI USING DLET 1165-PCB DLI-IO-AREA                         
108600     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
108700     PERFORM IMS-STATUSKONTROLL                                           
108800     .                                                                    
108900     SKIP3                                                                
109000 IMS-STATUSKONTROLL SECTION.                                              
109100     SET STATUS-IX TO 1                                                   
109200     SEARCH GODK-STATUS                                                   
109300       AT END                                                             
109400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
109500         DELIMITED BY SIZE INTO FELTEXT                                   
109600         CALL FELLOG                                                      
109700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
109800         CONTINUE                                                         
109900     END-SEARCH                                                           
110000     .                                                                    
