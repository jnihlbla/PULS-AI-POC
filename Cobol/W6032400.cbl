000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6032400.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   05/02/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR OCH UPPDATERAR GODKÄNDA USERID FÖR SKROT                   
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDR5                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W6T324                                              
001400*        MID:         W6I32401                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W6O32401                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6032400'.            
002600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002700 77  JA                          PIC X       VALUE 'J'.                   
002800 77  NEJ                         PIC X       VALUE 'N'.                   
002900 77  IX                          PIC S9(3)   VALUE +0 COMP-3.             
003000 77  MAX-IX                      PIC S9(3)   VALUE +5 COMP-3.             
003100 77  SW-DLET                     PIC X       VALUE SPACE.                 
003200 77  SW-BORTTAG                  PIC X       VALUE SPACE.                 
003300 77  SW-PA-SKROTORD              PIC X       VALUE SPACE.                 
003400 77  SPAR-DLET-IDUSER-GODK       PIC X(8)    VALUE SPACE.                 
003500 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003600 77  WS-KVANTAL                  PIC 9(7)    VALUE ZERO.                  
003700 77  NYUPPL                      PIC X       VALUE 'I'.                   
003800 77  UPPDAT                      PIC X       VALUE 'R'.                   
003900 77  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
004000 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004100 77  WS-A                        PIC X(1)    VALUE SPACE.                 
004200 77  WS-B                        PIC X(1)    VALUE SPACE.                 
004300                                                                          
004400 77  WS-BEHANDLING               PIC X.                                   
004500   88  BEH-NYUPPL                        VALUE 'I'.                       
004600   88  BEH-UPPDAT                        VALUE 'R'.                       
004700                                                                          
004800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004900     88  INDATA-OK                           VALUE 'J'.                   
005000     88  INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '6324'.                
005800***  88  GODK-MID                            VALUE '6321' '6322'          
005900***                                                '6323' '6324'.         
006000     88  HELP-MID                            VALUE '0551'.                
006100     EJECT                                                                
006200*01        -COPY WDGX6328 -PRE SPAR-                                      
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008200     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008700     SKIP3                                                                
008800*01 -COPY WMSGINIT                                                        
008900     EJECT                                                                
009000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009100*                                                                         
009200 01  SPAR-AREA.                                                           
009300     03  SPAR-IDTRANS           PIC X(4)    VALUE '6324'.                 
009400     03  SPAR-SUBEL-ENTER       PIC 9(7).                                 
009500     03  SPAR-SUBEL-NEXT        PIC 9(7).                                 
009600     03  SPAR-IDUSER-GODK-ENTER PIC X(8).                                 
009700     03  SPAR-IDUSER-GODK-NEXT  PIC X(8).                                 
009800     03  FILLER                 PIC X(1000) VALUE SPACE.                  
009900     EJECT                                                                
010000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010300     SKIP3                                                                
010400*01  MID -COPY W6I32401                                                   
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010700     SKIP3                                                                
010800*01  -COPY WMSGAREA                                                       
010900     EJECT                                                                
011000     03  MOD REDEFINES MSG-AREA.                                          
011100*      05  -COPY W6O32401                                                 
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011400     SKIP3                                                                
011500*01  -COPY WMFSAREA                                                       
011600     EJECT                                                                
011700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012000     SKIP3                                                                
012100 01  NYCKLAR-TILL-DLI.                                                    
012200     03  W-WDGXKEY-X.                                                     
012300         05  FILLER             PIC X(4)    VALUE '6327'.                 
012400         05  W-KDARBTYP-6327    PIC X(8)    VALUE SPACE.                  
012500         05  W-IDDC-6327        PIC X(2)    VALUE SPACE.                  
012600         05  FILLER             PIC X(16)   VALUE LOW-VALUE.              
012700     03  W-IDUSER-GODK-X.                                                 
012800         05  W-IDUSER-GODK      PIC X(8)       VALUE SPACE.               
012900     03  W-KY6328-MIN-X.                                                  
013000         05  W-SUBEL-MIN        PIC 9(7)       VALUE ZERO.                
013100         05  W-IDUSER-GODK-MIN  PIC X(8)       VALUE LOW-VALUE.           
013200     03  W-KY6328-MAX-X.                                                  
013300         05  W-SUBEL-MAX        PIC 9(7)                                  
013400                                  VALUE 9999999.                          
013500         05  W-IDUSER-GODK-MAX  PIC X(8)       VALUE HIGH-VALUE.          
013600     03  W-IDDC-B6-X.                                                     
013700         05  W-IDDC-B6          PIC X(2)       VALUE SPACE.               
013800     03  W-WDGXKEY-6321.                                                  
013900         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
014000         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
014100         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
014200     03  W-DASKROT9-X.                                                    
014300         05  W-DASKROT9      PIC 9(8)   VALUE ZERO.                       
014400     03  W-DASKROT9-MIN-X.                                                
014500         05  W-DASKROT9-MIN  PIC 9(8)   VALUE ZERO.                       
014600     03  W-DASKROT9-MAX-X.                                                
014700         05  W-DASKROT9-MAX  PIC 9(8)   VALUE 99999999.                   
014800     03  W-KY6324-KVAL-X.                                                 
014900         05  W-IDARTNR-KVAL  PIC S9(9)  VALUE ZERO COMP-3.                
015000         05  W-IDDC-KVAL     PIC X(2)   VALUE SPACE.                      
015100         05  W-KDSTASKR-KVAL PIC S9     VALUE 2 COMP-3.                   
015200     EJECT                                                                
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FINNS                       VALUE '  '.                  
015600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015800     SKIP2                                                                
015900 01  GODK-STATUSKODER.                                                    
016000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016100     SKIP3                                                                
016200 01  SSA1                        PIC X(64).                               
016300 01  SSA2                        PIC X(64).                               
016400 01  SSA3                        PIC X(64).                               
016500     EJECT                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000                                                                          
017100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
017200 01  DLI-IO-WDR501.                                                       
017300*    03  -COPY WDGX6327.                                                  
017400     EJECT                                                                
017500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
017600 01  DLI-IO-WDGX6328.                                                     
017700*    03  -COPY WDGX6328                                                   
017800     EJECT                                                                
017900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018000 01   DLI-IO-AREA-B601.                                                   
018100*     03  -COPY WDB601                                                    
018200     EJECT                                                                
018300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
018400 01  DLI-IO-WDR501-6321.                                                  
018500*    03  -COPY WDGX6321                                                   
018600     EJECT                                                                
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
018800 01  DLI-IO-WDGX6322.                                                     
018900*    03  -COPY WDGX6322                                                   
019000     EJECT                                                                
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
019200 01  DLI-IO-WDGX6324.                                                     
019300*    03  -COPY WDGX6324                                                   
019400     EJECT                                                                
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6326'.                    
019600 01  DLI-IO-WDGX6326.                                                     
019700*    03  -COPY WDGX6326                                                   
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000*01  -COPY W0009   -PRE MSG-                                              
020100     EJECT                                                                
020200*01  -COPY W0008   -PRE WDP7-                                             
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008   -PRE WDR5-                                             
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800*01  -COPY W0008   -PRE WDB6-                                             
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008   -PRE 6321-                                             
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDR5-PCB WDB6-PCB             
021500                           6321-PCB.                                      
021600 MAIN SECTION.                                                            
021700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR5-PCB WDB6-PCB             
021800                           6321-PCB.                                      
021900                                                                          
022000     PERFORM IMS-GET-MSG                                                  
022100     IF SEGMENT-FINNS                                                     
022200        PERFORM A-INIT                                                    
022300        PERFORM B-KOLLA-NYCKLAR                                           
022400        IF NYCKLAR-OK                                                     
022500           IF MFS-UPDATE                                                  
022600              PERFORM G-KOLLA-INPUT                                       
022700*             CALL FELLOG                                                 
022800              IF INDATA-OK                                                
022900                 PERFORM H-UPPDATERA                                      
023000              END-IF                                                      
023100           ELSE                                                           
023200              IF MFS-FIRST                                                
023300                 PERFORM C-FOERSTA-SIDA                                   
023400              ELSE                                                        
023500                 IF MFS-NEXT                                              
023600                    PERFORM D-NAESTA-SIDA                                 
023700                 ELSE                                                     
023800                    PERFORM E-SAMMA-SIDA                                  
023900                 END-IF                                                   
024000              END-IF                                                      
024100           END-IF                                                         
024200           IF INDATA-OK                                                   
024300              PERFORM F-LAES-VISA-INFO                                    
024400           END-IF                                                         
024500        END-IF                                                            
024600        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O32401 + 4                     
024700        PERFORM IMS-INSERT-MSG                                            
024800     END-IF                                                               
024900                                                                          
025000     MOVE ZERO TO RETURN-CODE                                             
025100     GOBACK                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 A-INIT SECTION.                                                          
025500                                                                          
025600     IF MSG-DUBBLA-TRANSKODER                                             
025700        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I32401                
025800        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
025900        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
026000     ELSE                                                                 
026100        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I32401                 
026200        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
026300        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
026400     END-IF                                                               
026500                                                                          
026600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026900                                                                          
027000     MOVE LOW-VALUE TO MSG-AREA                                           
027100     MOVE 'W6O32401' TO MFS-IDMOD                                         
027200     MOVE '6324' TO MOD-IDTRANS                                           
027300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027400                                                                          
027500     IF EGEN-MID                                                          
027600        CONTINUE                                                          
027700     ELSE                                                                 
027800        MOVE SPACE TO MFS-KDTRTYP                                         
027900        MOVE '7' TO MFS-IDPFK                                             
028000     END-IF                                                               
028100     MOVE NEJ    TO SW-PA-SKROTORD                                        
028200                                                                          
028300     ACCEPT DAGENS-DATUM FROM DATE                                        
028400     .                                                                    
028500     EJECT                                                                
028600 B-KOLLA-NYCKLAR SECTION.                                                 
028700                                                                          
028800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028900     MOVE '001'             TO MSGI-KDCALL                                
029000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029200     MOVE '6324'            TO MSGI-IDTRANS                               
029300     IF EGEN-MID                                                          
029400        MOVE MID-KDARBTYP-IN   TO MSGI-KDARBTYP                           
029500        MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                           
029600     END-IF                                                               
029700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
029800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
029900     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
030000                                                                          
030100     MOVE JA TO NYCKLAR-SW                                                
030200                                                                          
030300     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-IN                              
030400     IF MID-KDARBTYP-IN NOT = ALL '+'                                     
030500        MOVE '7'   TO MFS-IDPFK                                           
030600        MOVE SPACE TO MFS-KDTRTYP                                         
030700     END-IF                                                               
030800     IF MSGI-KDARBTYP = 'ANSK' OR 'ESC' OR 'QUAL' OR 'DISC' OR            
030810                        'ERS '                                            
030900        CONTINUE                                                          
031000     ELSE                                                                 
031100        MOVE NEJ TO NYCKLAR-SW                                            
031200     END-IF                                                               
031300                                                                          
031400     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
031500     IF MID-IDDC-IN NOT = ALL '+'                                         
031600        MOVE '7'   TO MFS-IDPFK                                           
031700        MOVE SPACE TO MFS-KDTRTYP                                         
031800     END-IF                                                               
031900     MOVE MSGI-IDDC-KEY TO WS-IDDC                                        
032000                           W-IDDC-B6                                      
032100     PERFORM IMS-GU-WDB601                                                
032200     IF SEGMENT-FINNS                                                     
032300        CONTINUE                                                          
032400     ELSE                                                                 
032500        MOVE NEJ TO NYCKLAR-SW                                            
032600     END-IF                                                               
032700                                                                          
032800***  IF GODK-MID OR NYCKLAR-OK                                            
032900     IF NYCKLAR-OK                                                        
033000        MOVE MSGI-KDARBTYP TO MOD-KDARBTYP-UT                             
033100        MOVE MSGI-IDDC-KEY TO MOD-IDDC-UT                                 
033200     ELSE                                                                 
033300        MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                           
033400                                MOD-IDDC-UT                               
033500     END-IF                                                               
033600                                                                          
033700     IF NYCKLAR-OK                                                        
033800        MOVE MSGI-KDARBTYP TO W-KDARBTYP-6327                             
033900                              W-6321-KDARBTYP                             
034000        MOVE MSGI-IDDC-KEY TO W-IDDC-6327                                 
034100                              W-IDDC-KVAL                                 
034200     END-IF                                                               
034300                                                                          
034400     IF NYCKLAR-FEL                                                       
034500        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
034600        CALL WMEDKONV USING MED-WMEDAREA                                  
034700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
034800        PERFORM MFS-RENSA-FAELT-IN                                        
034900        PERFORM MFS-RENSA-FAELT-UT                                        
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 C-FOERSTA-SIDA SECTION.                                                  
035400                                                                          
035500     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
035600     CALL WMEDKONV USING MED-WMEDAREA                                     
035700     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
035800                                                                          
035900     PERFORM MFS-RENSA-FAELT-IN                                           
036000     MOVE ZERO TO W-SUBEL-MIN                                             
036100     MOVE LOW-VALUE TO W-IDUSER-GODK-MIN                                  
036200     .                                                                    
036300     EJECT                                                                
036400 D-NAESTA-SIDA SECTION.                                                   
036500                                                                          
036600     IF SPAR-IDTRANS = '6324'                                             
036700        MOVE SPAR-SUBEL-NEXT TO W-SUBEL-MIN                               
036800        MOVE SPAR-IDUSER-GODK-NEXT TO W-IDUSER-GODK-MIN                   
036900     ELSE                                                                 
037000        MOVE MFS-RENSA-FAELT TO W-SUBEL-MIN                               
037100                                W-IDUSER-GODK-MIN                         
037200     END-IF                                                               
037300     PERFORM MFS-RENSA-FAELT-IN                                           
037400     .                                                                    
037500     EJECT                                                                
037600 E-SAMMA-SIDA SECTION.                                                    
037700                                                                          
037800     IF SPAR-IDTRANS = '6324'                                             
037900        MOVE SPAR-SUBEL-ENTER TO W-SUBEL-MIN                              
038000        MOVE SPAR-IDUSER-GODK-ENTER TO W-IDUSER-GODK-MIN                  
038100        IF MID-INPUT = ALL '+'                                            
038200        AND MID-KDCMDVAL(1) = ALL '+'                                     
038300        AND MID-KDCMDVAL(2) = ALL '+'                                     
038400        AND MID-KDCMDVAL(3) = ALL '+'                                     
038500        AND MID-KDCMDVAL(4) = ALL '+'                                     
038600        AND MID-KDCMDVAL(5) = ALL '+'                                     
038700           PERFORM MFS-RENSA-FAELT-IN                                     
038800        ELSE                                                              
038900           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
039000           CALL WMEDKONV USING MED-WMEDAREA                               
039100           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
039200           PERFORM EA-MID-INDATA-TILL-MOD                                 
039300        END-IF                                                            
039400     ELSE                                                                 
039500        PERFORM MFS-RENSA-FAELT-IN                                        
039600        MOVE MFS-RENSA-FAELT TO W-SUBEL-MIN                               
039700                                W-IDUSER-GODK-MIN                         
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 EA-MID-INDATA-TILL-MOD SECTION.                                          
040200                                                                          
040300     IF MID-BEANST-GODK NOT = ALL '+'                                     
040400        MOVE MFS-ROER-EJ-FAELT  TO MOD-BEANST-GODK-IN                     
040500     ELSE                                                                 
040600        MOVE MFS-RENSA-FAELT    TO MOD-BEANST-GODK-IN                     
040700     END-IF                                                               
040800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEANST-GODK-ATTR                   
040900                                                                          
041000     IF MID-IDUSER-GODK-IN NOT = ALL '+'                                  
041100        MOVE MFS-ROER-EJ-FAELT  TO MOD-IDUSER-GODK-IN                     
041200     ELSE                                                                 
041300        MOVE MFS-RENSA-FAELT    TO MOD-IDUSER-GODK-IN                     
041400     END-IF                                                               
041500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDUSER-GODK-ATTR                   
041600                                                                          
041700     IF MID-KVANTAL NOT = ALL '+'                                         
041800        MOVE MFS-ROER-EJ-FAELT  TO MOD-KVANTAL-IN                         
041900     ELSE                                                                 
042000        MOVE MFS-RENSA-FAELT    TO MOD-KVANTAL-IN                         
042100     END-IF                                                               
042200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-ATTR                       
042300                                                                          
042400     IF MID-IDUSER-PRI NOT = ALL '+'                                      
042500        MOVE MFS-ROER-EJ-FAELT  TO MOD-IDUSER-PRI-IN                      
042600     ELSE                                                                 
042700        MOVE MFS-RENSA-FAELT    TO MOD-IDUSER-PRI-IN                      
042800     END-IF                                                               
042900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDUSER-PRI-ATTR                    
043000                                                                          
043100     IF MID-IDMAIL NOT = ALL '+'                                          
043200        MOVE MFS-ROER-EJ-FAELT  TO MOD-IDMAIL-IN                          
043300     ELSE                                                                 
043400        MOVE MFS-RENSA-FAELT    TO MOD-IDMAIL-IN                          
043500     END-IF                                                               
043600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL-ATTR                        
043700                                                                          
043800     MOVE +1 TO IX                                                        
043900     PERFORM UNTIL IX > MAX-IX                                            
044000        IF MID-KDCMDVAL(IX) NOT = ALL '+'                                 
044100           MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-IN(IX)                  
044200        ELSE                                                              
044300           MOVE MFS-RENSA-FAELT   TO MOD-KDCMDVAL-IN(IX)                  
044400        END-IF                                                            
044500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR(IX)               
044600        ADD +1 TO IX                                                      
044700     END-PERFORM                                                          
044800     .                                                                    
044900     EJECT                                                                
045000 F-LAES-VISA-INFO SECTION.                                                
045100                                                                          
045200     PERFORM FA-LAES-GRUNDDATA                                            
045300                                                                          
045400     IF SEGMENT-SAKNAS                                                    
045500        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
045600        CALL WMEDKONV USING MED-WMEDAREA                                  
045700        IF MOD-TEMFSFEL = SPACE                                           
045800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
045900        ELSE                                                              
046000           MOVE MED-MFSFEL TO MOD-TEMFSINF                                
046100        END-IF                                                            
046200        PERFORM MFS-RENSA-FAELT-UT                                        
046300     ELSE                                                                 
046400        MOVE +1 TO IX                                                     
046500        PERFORM FB-LAES-RADDATA                                           
046600        IF SEGMENT-FINNS                                                  
046700           MOVE 6328-SUBEL      TO SPAR-SUBEL-ENTER                       
046800           MOVE 6328-IDUSER-GODK TO SPAR-IDUSER-GODK-ENTER                
046900        ELSE                                                              
047000           MOVE W-SUBEL-MIN         TO SPAR-SUBEL-ENTER                   
047100           MOVE W-IDUSER-GODK-MIN   TO SPAR-IDUSER-GODK-ENTER             
047200        END-IF                                                            
047300                                                                          
047400        PERFORM UNTIL IX > MAX-IX                                         
047500           IF SEGMENT-FINNS                                               
047600              MOVE 6328-SUBEL       TO MOD-KVANTAL(IX)                    
047700              MOVE 6328-IDUSER-GODK TO MOD-IDUSER-GODK(IX)                
047800              MOVE 6328-BEANST-GODK TO MOD-BEANST-GODK(IX)                
047900              MOVE 6328-IDMAIL      TO MOD-IDMAIL(IX)                     
048000              MOVE 6328-IDUSER-OREG TO MOD-IDUSER-OREG(IX)                
048100              MOVE 6328-TIUPPDAT    TO MOD-TIAAMMDD(IX)                   
048200              MOVE 6328-IDUSER-PRI  TO MOD-IDUSER-PRI(IX)                 
048300              PERFORM FB-LAES-RADDATA                                     
048400           ELSE                                                           
048500              MOVE MFS-RENSA-FAELT TO MOD-KVANTAL(IX)                     
048600                                      MOD-IDUSER-GODK(IX)                 
048700                                      MOD-BEANST-GODK(IX)                 
048800                                      MOD-IDMAIL(IX)                      
048900                                      MOD-IDUSER-OREG(IX)                 
049000                                      MOD-TIAAMMDD(IX)                    
049100                                      MOD-IDUSER-PRI(IX)                  
049200                                                                          
049300           END-IF                                                         
049400           ADD +1 TO IX                                                   
049500        END-PERFORM                                                       
049600                                                                          
049700        IF SEGMENT-FINNS                                                  
049800           MOVE 6328-SUBEL      TO SPAR-SUBEL-NEXT                        
049900           MOVE 6328-IDUSER-GODK TO SPAR-IDUSER-GODK-NEXT                 
050000           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
050100           CALL WMEDKONV USING MED-WMEDAREA                               
050200           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
050300        ELSE                                                              
050400           MOVE W-SUBEL-MIN       TO SPAR-SUBEL-NEXT                      
050500           MOVE W-IDUSER-GODK-MIN TO SPAR-IDUSER-GODK-NEXT                
050600        END-IF                                                            
050700                                                                          
050800        MOVE '002'      TO MSGI-KDCALL                                    
050900        MOVE '6324'     TO SPAR-IDTRANS                                   
051000        MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                 
051100        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
051200      END-IF                                                              
051300     .                                                                    
051400     EJECT                                                                
051500 FA-LAES-GRUNDDATA SECTION.                                               
051600                                                                          
051700     PERFORM IMS-GET-WDR501-6327                                          
051800     .                                                                    
051900     EJECT                                                                
052000 FB-LAES-RADDATA SECTION.                                                 
052100                                                                          
052200     PERFORM IMS-GNP-WDGX6328                                             
052300     .                                                                    
052400     EJECT                                                                
052500 G-KOLLA-INPUT SECTION.                                                   
052600                                                                          
052700     MOVE JA TO INDATA-SW                                                 
052800                                                                          
052900     IF MID-INPUT = ALL '+'                                               
053000     AND MID-KDCMDVAL(1) = ALL '+'                                        
053100     AND MID-KDCMDVAL(2) = ALL '+'                                        
053200     AND MID-KDCMDVAL(3) = ALL '+'                                        
053300     AND MID-KDCMDVAL(4) = ALL '+'                                        
053400     AND MID-KDCMDVAL(5) = ALL '+'                                        
053500        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
053600        CALL WMEDKONV USING MED-WMEDAREA                                  
053700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
053800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
053900        PERFORM MFS-ROER-EJ-FAELT-UT                                      
054000        MOVE NEJ TO INDATA-SW                                             
054100     ELSE                                                                 
054200        MOVE +1 TO IX                                                     
054300        MOVE NEJ TO SW-BORTTAG                                            
054400        PERFORM UNTIL IX > MAX-IX                                         
054500           IF MID-KDCMDVAL(IX) = ALL '+' OR SPACE                         
054600              CONTINUE                                                    
054700           ELSE                                                           
054800             MOVE JA TO SW-BORTTAG                                        
054900             IF MID-KDCMDVAL(IX) = 'D'                                    
055000                MOVE MFS-ALFA-FAELT-RAETT                                 
055100                                   TO MOD-KDCMDVAL-ATTR(IX)               
055200                PERFORM IMS-GET-WDR501-6327                               
055300                IF SEGMENT-FINNS                                          
055400                   MOVE MID-IDUSER-GODK(IX) TO W-IDUSER-GODK              
055500                                               WS-IDUSER                  
055600                   PERFORM IMS-GET-WDGX6328                               
055700                   IF SEGMENT-FINNS                                       
055800                      MOVE W-IDUSER-GODK TO SPAR-DLET-IDUSER-GODK         
055900                      PERFORM IMS-GET-WDR501-6327                         
056000                      PERFORM IMS-LAS-WDGX6328                            
056100                      PERFORM UNTIL SEGMENT-SAKNAS                        
056200                         IF 6328-IDUSER-PRI = W-IDUSER-GODK               
056300                            MOVE MFS-ALFA-FAELT-FEL                       
056400                                      TO MOD-KDCMDVAL-ATTR(IX)            
056500                            MOVE NEJ TO INDATA-SW                         
056600                         END-IF                                           
056700                         PERFORM IMS-LAS-WDGX6328                         
056800                      END-PERFORM                                         
057300                      PERFORM GE-KOLLA-SKROTORDER                         
057400                      IF SW-PA-SKROTORD = JA                              
057500                        MOVE 'NON APPROVED ISSUE EXIST'                   
057600                                    TO MOD-TEMFSINF                       
057700                        MOVE MFS-ALFA-FAELT-FEL                           
057800                                    TO MOD-KDCMDVAL-ATTR(IX)              
057900                        MOVE NEJ TO INDATA-SW                             
058000                      END-IF                                              
058200                   ELSE                                                   
058300                      MOVE MFS-ALFA-FAELT-FEL                             
058400                                    TO MOD-KDCMDVAL-ATTR(IX)              
058500                      MOVE NEJ TO INDATA-SW                               
058600                   END-IF                                                 
058700                ELSE                                                      
058800                   MOVE MFS-ALFA-FAELT-FEL                                
058900                                    TO MOD-KDCMDVAL-ATTR(IX)              
059000                   MOVE NEJ TO INDATA-SW                                  
059100                END-IF                                                    
059200             ELSE                                                         
059300                MOVE MFS-ALFA-FAELT-FEL                                   
059400                                    TO MOD-KDCMDVAL-ATTR(IX)              
059500                MOVE NEJ TO INDATA-SW                                     
059600             END-IF                                                       
059700          END-IF                                                          
059800          ADD +1 TO IX                                                    
059900        END-PERFORM                                                       
060000                                                                          
060100        PERFORM GA-KOLLA-RADER                                            
060200        IF INDATA-OK                                                      
060300           PERFORM IMS-GET-WDR501-6327                                    
060400           IF SEGMENT-FINNS                                               
060500              MOVE MID-IDUSER-GODK-IN TO W-IDUSER-GODK                    
060600              PERFORM IMS-GET-WDGX6328                                    
060700              IF SEGMENT-FINNS                                            
060800                 PERFORM GC-KOLLA-UPPDAT                                  
060900              ELSE                                                        
061000                 PERFORM GB-KOLLA-NYUPPLAGG                               
061100              END-IF                                                      
061200           ELSE                                                           
061300              PERFORM GB-KOLLA-NYUPPLAGG                                  
061400           END-IF                                                         
061500        END-IF                                                            
061600                                                                          
061700        IF INDATA-FEL                                                     
061800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
061900           CALL WMEDKONV USING MED-WMEDAREA                               
062000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
062100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
062200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
062300        END-IF                                                            
062400                                                                          
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 GA-KOLLA-RADER SECTION.                                                  
062900                                                                          
063000     IF MID-BEANST-GODK NOT = ALL '+'                                     
063100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEANST-GODK-ATTR                 
063200     END-IF                                                               
063300                                                                          
063400     IF MID-IDUSER-GODK-IN NOT = ALL '+'                                  
063500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-GODK-ATTR                 
063600     END-IF                                                               
063700                                                                          
063800     IF MID-IDUSER-PRI NOT = ALL '+'                                      
063900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-PRI-ATTR                  
064000     END-IF                                                               
064100                                                                          
064200     IF MID-IDMAIL NOT = ALL '+'                                          
064300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL-ATTR                      
064400     END-IF                                                               
064500                                                                          
064600     IF MID-KVANTAL NOT = ALL '+'                                         
064700        IF MID-KVANTAL NOT NUMERIC                                        
064800           MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR                     
064900           MOVE NEJ TO INDATA-SW                                          
065000        ELSE                                                              
065100           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-ATTR                   
065200        END-IF                                                            
065300     END-IF                                                               
065400                                                                          
065500     .                                                                    
065600     EJECT                                                                
065700 GB-KOLLA-NYUPPLAGG SECTION.                                              
065800                                                                          
065900     IF MID-INPUT = ALL '+' AND SW-BORTTAG = NEJ                          
066000        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-GODK-ATTR                   
066100        MOVE NEJ TO INDATA-SW                                             
066200     ELSE                                                                 
066300        IF (MID-BEANST-GODK NOT = ALL '+')                                
066400        OR (MID-IDUSER-GODK-IN NOT = ALL '+')                             
066500        OR (MID-KVANTAL NOT = ALL '+')                                    
066600        OR (MID-IDMAIL NOT = ALL '+')                                     
066700                                                                          
066800           IF MID-BEANST-GODK = ALL '+' OR SPACE                          
066900              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEANST-GODK-ATTR             
067000              MOVE NEJ TO INDATA-SW                                       
067100           END-IF                                                         
067200                                                                          
067300           IF MID-IDUSER-GODK-IN = ALL '+' OR SPACE                       
067400              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-GODK-ATTR             
067500              MOVE NEJ TO INDATA-SW                                       
067600           END-IF                                                         
067700                                                                          
067800           IF MID-KVANTAL = ALL '+' OR SPACE                              
067900              MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR                  
068000              MOVE NEJ TO INDATA-SW                                       
068100           ELSE                                                           
068200              IF MID-KVANTAL NUMERIC                                      
068300                 MOVE MID-KVANTAL TO WS-KVANTAL                           
068400              ELSE                                                        
068500                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR               
068600                 MOVE NEJ TO INDATA-SW                                    
068700              END-IF                                                      
068800           END-IF                                                         
068900                                                                          
069000           IF MID-IDMAIL = ALL '+' OR SPACE                               
069100              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMAIL-ATTR                  
069200              MOVE NEJ TO INDATA-SW                                       
069300           END-IF                                                         
069400                                                                          
069500           IF MID-IDUSER-PRI = ALL '+'                                    
069600              CONTINUE                                                    
069700           ELSE                                                           
069800              PERFORM IMS-GET-WDR501-6327                                 
069900              IF SEGMENT-FINNS                                            
070000                 MOVE MID-IDUSER-PRI TO W-IDUSER-GODK                     
070100                 PERFORM IMS-GET-WDGX6328                                 
070200                 IF SEGMENT-SAKNAS                                        
070300                    MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-PRI-ATTR        
070400                    MOVE NEJ TO INDATA-SW                                 
070500                 ELSE                                                     
070600                    IF INDATA-OK AND WS-KVANTAL NUMERIC                   
070700                       IF 6328-SUBEL < WS-KVANTAL                         
070800                          MOVE MFS-ALFA-FAELT-FEL                         
070900                                    TO MOD-IDUSER-PRI-ATTR                
071000                          MOVE NEJ TO INDATA-SW                           
071100                       END-IF                                             
071200                    END-IF                                                
071300                 END-IF                                                   
071400              ELSE                                                        
071500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-PRI-ATTR           
071600                 MOVE NEJ TO INDATA-SW                                    
071700              END-IF                                                      
071800           END-IF                                                         
071900                                                                          
072000        END-IF                                                            
072100     END-IF                                                               
072200     .                                                                    
072300     EJECT                                                                
072400 GC-KOLLA-UPPDAT SECTION.                                                 
072500                                                                          
072600      IF MID-BEANST-GODK = ALL '+'                                        
072700      AND MID-KVANTAL = ALL '+'                                           
072800      AND MID-IDMAIL = ALL '+'                                            
072900      AND MID-IDUSER-PRI = ALL '+'                                        
073000      AND SW-BORTTAG = NEJ                                                
073100         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-GODK-ATTR                  
073200         MOVE NEJ TO INDATA-SW                                            
073300      ELSE                                                                
073400                                                                          
073500         MOVE SPACE TO WS-BEHANDLING                                      
073600                                                                          
073700         IF MID-KVANTAL NOT = ALL '+'                                     
073800            MOVE MID-KVANTAL TO WS-KVANTAL                                
073900            IF WS-KVANTAL NOT = 6328-SUBEL                                
074000               MOVE WS-KVANTAL TO SPAR-6328-SUBEL                         
074100               MOVE NYUPPL TO WS-BEHANDLING                               
074200            ELSE                                                          
074300               MOVE WS-KVANTAL TO SPAR-6328-SUBEL                         
074400            END-IF                                                        
074500         ELSE                                                             
074600            MOVE 6328-SUBEL TO SPAR-6328-SUBEL                            
074700         END-IF                                                           
074800                                                                          
074900         IF MID-IDUSER-GODK-IN = ALL '+' OR SPACE                         
075000            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-GODK-ATTR               
075100            MOVE NEJ TO INDATA-SW                                         
075200         ELSE                                                             
075300            MOVE MID-IDUSER-GODK-IN TO SPAR-6328-IDUSER-GODK              
075400         END-IF                                                           
075500                                                                          
075600         IF MID-BEANST-GODK = ALL '+'                                     
075700            MOVE 6328-BEANST-GODK TO SPAR-6328-BEANST-GODK                
075800         ELSE                                                             
075900            MOVE MID-BEANST-GODK TO SPAR-6328-BEANST-GODK                 
076000            IF WS-BEHANDLING = SPACE                                      
076100               MOVE UPPDAT TO WS-BEHANDLING                               
076200            END-IF                                                        
076300         END-IF                                                           
076400                                                                          
076500         IF MID-IDMAIL = ALL '+'                                          
076600            MOVE 6328-IDMAIL TO SPAR-6328-IDMAIL                          
076700         ELSE                                                             
076800            MOVE MID-IDMAIL TO SPAR-6328-IDMAIL                           
076900            IF WS-BEHANDLING = SPACE                                      
077000               MOVE UPPDAT TO WS-BEHANDLING                               
077100            END-IF                                                        
077200         END-IF                                                           
077300                                                                          
077400         IF MID-IDUSER-PRI = SPACE                                        
077500            MOVE SPACE TO SPAR-6328-IDUSER-PRI                            
077600         ELSE                                                             
077700            IF MID-IDUSER-PRI = ALL '+'                                   
077800               MOVE 6328-IDUSER-PRI TO SPAR-6328-IDUSER-PRI               
077900            ELSE                                                          
078000               PERFORM IMS-GET-WDR501-6327                                
078100               IF SEGMENT-FINNS                                           
078200                  MOVE MID-IDUSER-PRI TO W-IDUSER-GODK                    
078300                  PERFORM IMS-GET-WDGX6328                                
078400                  IF SEGMENT-SAKNAS                                       
078500                     MOVE MFS-ALFA-FAELT-FEL                              
078600                                       TO MOD-IDUSER-PRI-ATTR             
078700                     MOVE NEJ TO INDATA-SW                                
078800                  ELSE                                                    
078900                     IF INDATA-OK AND SPAR-6328-SUBEL NUMERIC             
079000                        IF 6328-SUBEL < SPAR-6328-SUBEL                   
079100                           MOVE MFS-ALFA-FAELT-FEL                        
079200                                     TO MOD-IDUSER-PRI-ATTR               
079300                           MOVE NEJ TO INDATA-SW                          
079400                        END-IF                                            
079500                     END-IF                                               
079600                     IF INDATA-OK                                         
079700                        MOVE MID-IDUSER-PRI                               
079800                                       TO SPAR-6328-IDUSER-PRI            
079900                        IF WS-BEHANDLING = SPACE                          
080000                           MOVE UPPDAT TO WS-BEHANDLING                   
080100                        END-IF                                            
080200                     END-IF                                               
080300                  END-IF                                                  
080400               ELSE                                                       
080500                  MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-PRI-ATTR          
080600                  MOVE NEJ TO INDATA-SW                                   
080700               END-IF                                                     
080800            END-IF                                                        
080900         END-IF                                                           
081000                                                                          
081100         IF INDATA-OK                                                     
081200            IF SPAR-6328-IDUSER-PRI = SPAR-6328-IDUSER-GODK               
081300               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-PRI-ATTR             
081400               MOVE NEJ TO INDATA-SW                                      
081500            END-IF                                                        
081600         END-IF                                                           
081700                                                                          
081800         MOVE MSG-SIGNON-USERID TO SPAR-6328-IDUSER-OREG                  
081900         MOVE DAGENS-DATUM      TO SPAR-6328-TIUPPDAT                     
082000                                                                          
082100     END-IF                                                               
082200     .                                                                    
082300     EJECT                                                                
082400 GE-KOLLA-SKROTORDER SECTION.                                             
082500                                                                          
082600     PERFORM IMS-GU-WDR501-6321                                           
082700     IF SEGMENT-FINNS                                                     
082710       PERFORM IMS-GNP-WDGX6322                                           
082800       PERFORM UNTIL SEGMENT-SAKNAS OR SW-PA-SKROTORD = JA                
082900         MOVE 6322-DASKROT9-BEORD  TO W-DASKROT9                          
083000         PERFORM IMS-GNP-WDGX6324                                         
083100         PERFORM UNTIL SEGMENT-SAKNAS OR SW-PA-SKROTORD = JA              
083200           IF 6324-IDDC = MSGI-IDDC-KEY                                   
083300             MOVE 6324-IDARTNR     TO W-IDARTNR-KVAL                      
083400             IF 6324-IDUSER = W-IDUSER-GODK                               
083500               MOVE JA  TO SW-PA-SKROTORD                                 
083600             END-IF                                                       
083700             PERFORM IMS-GNP-WDGX6326-LAST                                
083800             IF 6326-IDUSER-GODK = W-IDUSER-GODK                          
083900               MOVE JA  TO SW-PA-SKROTORD                                 
084000             END-IF                                                       
084010           END-IF                                                         
084100           PERFORM IMS-GNP-WDGX6324                                       
084200         END-PERFORM                                                      
084300         PERFORM IMS-GNP-WDGX6322                                         
084400       END-PERFORM                                                        
084410     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700 H-UPPDATERA SECTION.                                                     
084800                                                                          
084900     MOVE NEJ TO SW-DLET                                                  
085000     MOVE +1 TO IX                                                        
085100     PERFORM UNTIL IX > MAX-IX                                            
085200        IF MID-KDCMDVAL(IX) NOT = ALL '+'                                 
085300           IF MID-KDCMDVAL(IX) = 'D'                                      
085400              PERFORM IMS-GET-WDR501-6327                                 
085500              MOVE MID-IDUSER-GODK(IX) TO W-IDUSER-GODK                   
085600              PERFORM IMS-GET-WDR501-6327                                 
085700              PERFORM IMS-GET-WDGX6328                                    
085800              PERFORM IMS-DLET-WDGX6328                                   
085900              MOVE JA TO SW-DLET                                          
086000           END-IF                                                         
086100        END-IF                                                            
086200        ADD +1 TO IX                                                      
086300     END-PERFORM                                                          
086400                                                                          
086500     IF SW-DLET = JA                                                      
086600        PERFORM HC-KOLLA-TOM-ROT                                          
086700     END-IF                                                               
086800                                                                          
086900     IF MID-INPUT NOT = ALL '+'                                           
087000        PERFORM IMS-GET-WDR501-6327                                       
087100        IF SEGMENT-FINNS                                                  
087200           MOVE MID-IDUSER-GODK-IN TO W-IDUSER-GODK                       
087300           PERFORM IMS-GET-WDGX6328                                       
087400           IF SEGMENT-FINNS                                               
087500              PERFORM HD-UPPDAT                                           
087600           ELSE                                                           
087700              PERFORM HA-NYUPPLAGG                                        
087800           END-IF                                                         
087900        ELSE                                                              
088000           PERFORM HB-NY-ROT                                              
088100           PERFORM HA-NYUPPLAGG                                           
088200        END-IF                                                            
088300     END-IF                                                               
088400                                                                          
088500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
088600     CALL WMEDKONV USING MED-WMEDAREA                                     
088700     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
088800     PERFORM MFS-FORM-ATTR                                                
088900     PERFORM MFS-RENSA-FAELT-IN                                           
089000     .                                                                    
089100     EJECT                                                                
089200 HA-NYUPPLAGG SECTION.                                                    
089300                                                                          
089400     IF MID-BEANST-GODK NOT = ALL '+'                                     
089500        MOVE MID-BEANST-GODK TO 6328-BEANST-GODK                          
089600     END-IF                                                               
089700                                                                          
089800     IF MID-IDUSER-GODK-IN NOT = ALL '+'                                  
089900        MOVE MID-IDUSER-GODK-IN TO 6328-IDUSER-GODK                       
090000     END-IF                                                               
090100                                                                          
090200     IF MID-KVANTAL NOT = ALL '+'                                         
090300        MOVE MID-KVANTAL TO WS-KVANTAL                                    
090400        MOVE WS-KVANTAL  TO 6328-SUBEL                                    
090500     END-IF                                                               
090600                                                                          
090700     IF MID-IDMAIL NOT = ALL '+'                                          
090800        MOVE MID-IDMAIL     TO 6328-IDMAIL                                
090900     END-IF                                                               
091000                                                                          
091100     IF MID-IDUSER-PRI NOT = ALL '+'                                      
091200        MOVE MID-IDUSER-PRI TO 6328-IDUSER-PRI                            
091300     ELSE                                                                 
091400        MOVE SPACE          TO 6328-IDUSER-PRI                            
091500     END-IF                                                               
091600                                                                          
091700     MOVE MSG-SIGNON-USERID TO 6328-IDUSER-OREG                           
091800     MOVE DAGENS-DATUM      TO 6328-TIUPPDAT                              
091900     PERFORM IMS-ISRT-WDGX6328                                            
092000     .                                                                    
092100     EJECT                                                                
092200 HB-NY-ROT SECTION.                                                       
092300                                                                          
092400     MOVE '6327'          TO 6327-IDHTYP                                  
092500     MOVE MSGI-KDARBTYP   TO 6327-KDARBTYP                                
092600     MOVE MSGI-IDDC-KEY   TO 6327-IDDC                                    
092700     PERFORM IMS-ISRT-WDR501-6327                                         
092800     .                                                                    
092900     EJECT                                                                
093000 HC-KOLLA-TOM-ROT SECTION.                                                
093100                                                                          
093200     PERFORM IMS-GET-WDR501-6327                                          
093300     MOVE LOW-VALUE    TO W-IDUSER-GODK-MIN                               
093400     MOVE ZERO         TO W-SUBEL-MIN                                     
093500     MOVE HIGH-VALUE   TO W-IDUSER-GODK-MAX                               
093600     MOVE 9999999      TO W-SUBEL-MAX                                     
093700     PERFORM IMS-GNP-WDGX6328                                             
093800     IF SEGMENT-SAKNAS                                                    
093900        PERFORM IMS-GET-WDR501-6327                                       
094000        PERFORM IMS-DLET-WDR501-6327                                      
094100     END-IF                                                               
094200     .                                                                    
094300     EJECT                                                                
094400 HD-UPPDAT SECTION.                                                       
094500                                                                          
094600     IF BEH-UPPDAT                                                        
094700        MOVE SPAR-6328-WDGX6328 TO 6328-WDGX6328                          
094800        PERFORM IMS-REPL-WDGX6328                                         
094900     ELSE                                                                 
095000        IF BEH-NYUPPL                                                     
095100           PERFORM IMS-DLET-WDGX6328                                      
095200           MOVE SPAR-6328-WDGX6328 TO 6328-WDGX6328                       
095300           PERFORM IMS-ISRT-WDGX6328                                      
095400        END-IF                                                            
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800 MFS-RENSA-FAELT-UT SECTION.                                              
095900                                                                          
096000     MOVE +1 TO IX                                                        
096100     PERFORM UNTIL IX > MAX-IX                                            
096200        MOVE MFS-RENSA-FAELT TO MOD-BEANST-GODK(IX)                       
096300                                MOD-IDUSER-GODK(IX)                       
096400                                MOD-KVANTAL(IX)                           
096500                                MOD-IDUSER-OREG(IX)                       
096600                                MOD-TIAAMMDD(IX)                          
096700                                MOD-IDUSER-PRI(IX)                        
096800                                MOD-IDMAIL(IX)                            
096900        ADD +1 TO IX                                                      
097000     END-PERFORM                                                          
097100     .                                                                    
097200     SKIP3                                                                
097300 MFS-RENSA-FAELT-IN SECTION.                                              
097400                                                                          
097500     MOVE +1 TO IX                                                        
097600     PERFORM UNTIL IX > MAX-IX                                            
097700        MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL-IN(IX)                       
097800        ADD +1 TO IX                                                      
097900     END-PERFORM                                                          
098000     MOVE MFS-RENSA-FAELT TO MOD-BEANST-GODK-IN                           
098100                             MOD-IDUSER-GODK-IN                           
098200                             MOD-KVANTAL-IN                               
098300                             MOD-IDUSER-PRI-IN                            
098400                             MOD-IDMAIL-IN                                
098500     .                                                                    
098600     EJECT                                                                
098700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
098800                                                                          
098900     MOVE +1 TO IX                                                        
099000     PERFORM UNTIL IX > MAX-IX                                            
099100        MOVE MFS-ROER-EJ-FAELT TO MOD-BEANST-GODK(IX)                     
099200                                  MOD-IDUSER-GODK(IX)                     
099300                                  MOD-KVANTAL(IX)                         
099400                                  MOD-IDUSER-OREG(IX)                     
099500                                  MOD-TIAAMMDD(IX)                        
099600                                  MOD-IDUSER-PRI(IX)                      
099700                                  MOD-IDMAIL(IX)                          
099800        ADD +1 TO IX                                                      
099900     END-PERFORM                                                          
100000     .                                                                    
100100     SKIP3                                                                
100200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
100300                                                                          
100400     MOVE +1 TO IX                                                        
100500     PERFORM UNTIL IX > MAX-IX                                            
100600        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-IN(IX)                     
100700        ADD +1 TO IX                                                      
100800     END-PERFORM                                                          
100900     MOVE MFS-ROER-EJ-FAELT TO MOD-BEANST-GODK-IN                         
101000                               MOD-IDUSER-GODK-IN                         
101100                               MOD-KVANTAL-IN                             
101200                               MOD-IDUSER-PRI-IN                          
101300                               MOD-IDMAIL-IN                              
101400     .                                                                    
101500     EJECT                                                                
101600 MFS-FORM-ATTR SECTION.                                                   
101700                                                                          
101800     MOVE +1 TO IX                                                        
101900     PERFORM UNTIL IX > MAX-IX                                            
102000        MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR(IX)                  
102100        ADD +1 TO IX                                                      
102200     END-PERFORM                                                          
102300     MOVE MFS-FORMATETS-ATTR TO MOD-BEANST-GODK-ATTR                      
102400                                MOD-IDUSER-GODK-ATTR                      
102500                                MOD-KVANTAL-ATTR                          
102600                                MOD-IDUSER-PRI-ATTR                       
102700                                MOD-IDMAIL-ATTR                           
102800     .                                                                    
102900     EJECT                                                                
103000* --- IMS SEKTIONER ---                                                   
103100     SKIP3                                                                
103200 IMS-GET-MSG SECTION.                                                     
103300     MOVE '  QC' TO GODK-STATUSKODER                                      
103400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
103500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103600     PERFORM IMS-STATUSKONTROLL                                           
103700     .                                                                    
103800     SKIP3                                                                
103900 IMS-INSERT-MSG SECTION.                                                  
104000     MOVE 'N' TO MFS-KDHUVOMR                                             
104100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
104200     MOVE SPACE TO GODK-STATUSKODER                                       
104300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
104400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
104500     PERFORM IMS-STATUSKONTROLL                                           
104600     .                                                                    
104700     EJECT                                                                
104800 IMS-GET-WDR501-6327 SECTION.                                             
104900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
105000          DELIMITED BY SIZE INTO SSA1                                     
105100     MOVE '  GE' TO GODK-STATUSKODER                                      
105200     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDR501 SSA1                   
105300     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
105400     PERFORM IMS-STATUSKONTROLL                                           
105500     .                                                                    
105600     SKIP3                                                                
105700 IMS-DLET-WDR501-6327 SECTION.                                            
105800     MOVE '  ' TO GODK-STATUSKODER                                        
105900     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDR501                       
106000     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
106100     PERFORM IMS-STATUSKONTROLL                                           
106200     .                                                                    
106300     SKIP3                                                                
106400 IMS-ISRT-WDR501-6327 SECTION.                                            
106500     MOVE 'WDR501  ' TO SSA1                                              
106600     MOVE '  II' TO GODK-STATUSKODER                                      
106700     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR501 SSA1                  
106800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
106900     PERFORM IMS-STATUSKONTROLL                                           
107000     .                                                                    
107100     EJECT                                                                
107200 IMS-GNP-WDGX6328 SECTION.                                                
107300     STRING 'WDGX6328(KY6328  =>' W-KY6328-MIN-X                          
107400                    '&KY6328  =<' W-KY6328-MAX-X ')'                      
107500          DELIMITED BY SIZE INTO SSA1                                     
107600     MOVE '  GE' TO GODK-STATUSKODER                                      
107700     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX6328 SSA1                
107800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
107900     PERFORM IMS-STATUSKONTROLL                                           
108000     .                                                                    
108100     SKIP3                                                                
108200 IMS-GET-WDGX6328 SECTION.                                                
108300     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK ')'                       
108400          DELIMITED BY SIZE INTO SSA1                                     
108500     MOVE '  GE' TO GODK-STATUSKODER                                      
108600     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX6328 SSA1                
108700     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
108800     PERFORM IMS-STATUSKONTROLL                                           
108900     .                                                                    
109000     SKIP3                                                                
109100 IMS-REPL-WDGX6328 SECTION.                                               
109200     MOVE '  ' TO GODK-STATUSKODER                                        
109300     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX6328                     
109400     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
109500     PERFORM IMS-STATUSKONTROLL                                           
109600     .                                                                    
109700     EJECT                                                                
109800 IMS-DLET-WDGX6328 SECTION.                                               
109900     MOVE '  ' TO GODK-STATUSKODER                                        
110000     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDGX6328                     
110100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
110200     PERFORM IMS-STATUSKONTROLL                                           
110300     .                                                                    
110400     SKIP3                                                                
110500 IMS-ISRT-WDGX6328 SECTION.                                               
110600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
110700          DELIMITED BY SIZE INTO SSA1                                     
110800     MOVE 'WDGX6328' TO SSA2                                              
110900     MOVE '  II' TO GODK-STATUSKODER                                      
111000     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX6328 SSA1 SSA2           
111100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
111200     PERFORM IMS-STATUSKONTROLL                                           
111300     .                                                                    
111400     SKIP3                                                                
111500 IMS-LAS-WDGX6328 SECTION.                                                
111600     MOVE 'WDGX6328' TO SSA1                                              
111700     MOVE '  GE' TO GODK-STATUSKODER                                      
111800     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX6328 SSA1                 
111900     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
112000     PERFORM IMS-STATUSKONTROLL                                           
112100     .                                                                    
112200     EJECT                                                                
112300 IMS-GU-WDB601    SECTION.                                                
112400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
112500          DELIMITED BY SIZE INTO SSA1                                     
112600     MOVE '  GE' TO GODK-STATUSKODER                                      
112700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
112800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     EJECT                                                                
113200 IMS-GU-WDR501-6321 SECTION.                                              
113300     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     MOVE 'GE  ' TO GODK-STATUSKODER                                      
113600     CALL CBLTDLI USING GU 6321-PCB DLI-IO-WDR501-6321 SSA1               
113700     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
113800     PERFORM IMS-STATUSKONTROLL                                           
113900     .                                                                    
114000     SKIP3                                                                
114100 IMS-GNP-WDGX6322 SECTION.                                                
114200*    STRING 'WDGX6322(DASKROT9=>' W-DASKROT9-MIN-X                        
114300*                   '&DASKROT9=<' W-DASKROT9-MAX-X ')'                    
114400*         DELIMITED BY SIZE INTO SSA1                                     
114500     MOVE 'WDGX6322 ' TO SSA1                                             
114600     MOVE '  GE' TO GODK-STATUSKODER                                      
114700     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6322 SSA1                 
114800     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
114900     PERFORM IMS-STATUSKONTROLL                                           
115000     .                                                                    
115100     EJECT                                                                
115200 IMS-GNP-WDGX6324 SECTION.                                                
115300     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
115400          DELIMITED BY SIZE INTO SSA1                                     
115500     MOVE 'WDGX6324 ' TO SSA2                                             
115600     MOVE '  GE' TO GODK-STATUSKODER                                      
115700     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2            
115800     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
115900     PERFORM IMS-STATUSKONTROLL                                           
116000     .                                                                    
116100     SKIP3                                                                
116200 IMS-GNP-WDGX6326-LAST SECTION.                                           
116300     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
116400          DELIMITED BY SIZE INTO SSA1                                     
116500     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
116600          DELIMITED BY SIZE INTO SSA2                                     
116700*    STRING 'WDGX6326*L(IDUSERGK= ' W-IDUSER-GODK-X ')'                   
116800*         DELIMITED BY SIZE INTO SSA3                                     
116900     MOVE 'WDGX6326*L'  TO SSA3                                           
117000     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6326 SSA1 SSA2            
117100                                                     SSA3                 
117200     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
117300     PERFORM IMS-STATUSKONTROLL                                           
117400     .                                                                    
117500     SKIP3                                                                
117600 IMS-STATUSKONTROLL SECTION.                                              
117700     SET STATUS-IX TO 1                                                   
117800     SEARCH GODK-STATUS                                                   
117900       AT END                                                             
118000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
118100         DELIMITED BY SIZE INTO FELTEXT                                   
118200         CALL FELLOG                                                      
118300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
118400         CONTINUE                                                         
118500     END-SEARCH                                                           
118600     .                                                                    
