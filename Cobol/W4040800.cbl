000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4040800.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   AUGUSTI   2012                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HANTERING AV DC-LEDTIDER    WDB6                                 
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDB6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T408                                              
001400*        MID:         W4I40801                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O40801                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4040800'.            
002600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002700                                                                          
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  YES                         PIC X       VALUE 'Y'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100 77  WS-DATUM                    PIC 9(6).                                
003200 77  WS-BOATSHLF                 PIC 9(6).                                
003300 77  MAX-ALLOW-LT                PIC 9(3)    VALUE 199.                   
003400 77  MAX-REAIRCO                 PIC 9(05)   VALUE 10000.                 
003500 77  MAX-RESSFAC                 PIC 9(02)   VALUE 10.                    
003500                                                                          
003500 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
003500 77  MAX-INDX                    PIC S9(4)   VALUE +7  COMP SYNC.         
003500                                                                          
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
003800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003900     88  INDATA-OK                           VALUE 'J'.                   
004000     88  INDATA-FEL                          VALUE 'N'.                   
004100                                                                          
004200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004300     88  NYCKLAR-OK                          VALUE 'J'.                   
004400     88  NYCKLAR-FEL                         VALUE 'N'.                   
004500                                                                          
004600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004700     88  EGEN-MID                            VALUE '4408'.                
004800     88  GODK-MID                            VALUE '4402' '4403'          
004900                                                   '4404' '0551'.         
005000     88  HELP-MID                            VALUE '0551'.                
005100                                                                          
005200 77  REF-SEG-SW                  PIC X       VALUE 'J'.                   
005300     88  REF-SEG-EXISTS                      VALUE 'J'.                   
005400     88  REF-SEG-MISSING                     VALUE 'N'.                   
005500                                                                          
005600     EJECT                                                                
005700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005800 01  GENERELLA-SUBPROGRAM.                                                
005900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
006500     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006600     EJECT                                                                
006700 01  W-REAIRCO                   PIC 9(05)V9(01).                         
006800 01  W-RESSFAC                   PIC 9V9(02).                             
006800 01  W-PRFRAKT                   PIC 9(07).                               
006800                                                                          
006900*   -COPY WDECAREA.                                                       
007000*01  -COPY WORKAREA                                                       
007100*01  -COPY WDAGAREA                                                       
007200*01  -COPY WWDC99                                                         
007300*01  -COPY WWDCKONS                                                       
007400                                                                          
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007800     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
008000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008200     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
008300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008400     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
008500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008700     EJECT                                                                
008800 01  MEDDELANDE.                                                          
008900     03  MED-1                  PIC X(30)                                 
009000         VALUE 'MAX LEADTIME 199              '.                          
009100     03  MED-2                  PIC X(30)                                 
009200         VALUE 'MAX AIR COST FACTOR 9999.9    '.                          
009200     03  MED-3                  PIC X(30)                                 
009200         VALUE 'MIN SAFETY STOCK FACTOR 0.01  '.                          
009200     03  MED-4                  PIC X(30)                                 
009200         VALUE 'MAX SAFETY STOCK FACTOR 9.99  '.                          
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009700     SKIP3                                                                
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
010000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010100*                                                                         
010200 01  SPAR-AREA.                                                           
010300     03  SPAR-IDTRANS           PIC X(4)    VALUE '4408'.                 
010400     03  SPAR-IDDC-REF          PIC X(2)    VALUE SPACE.                  
010500     EJECT                                                                
010600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010900     SKIP3                                                                
011000*01  MID -COPY W4I40801                                                   
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011300     SKIP3                                                                
011400*01  -COPY WMSGAREA                                                       
011500     EJECT                                                                
011600     03  MOD REDEFINES MSG-AREA.                                          
011700*      05  -COPY W4O40801                                                 
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012000     SKIP3                                                                
012100*01  -COPY WMFSAREA                                                       
012200     EJECT                                                                
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600     SKIP3                                                                
012700 01  NYCKLAR-TILL-DLI.                                                    
012800     03  W-WDB615KY-X.                                                    
012900         05  W-IDTRANS-B6        PIC X(4)    VALUE SPACE.                 
013000         05  W-IDDC-REF-B6       PIC X(2)    VALUE SPACE.                 
013100     03  W-IDDC-X.                                                        
013200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013300     03  W-IDDC-REF-X.                                                    
013400         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
013500     SKIP2                                                                
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FINNS                       VALUE '  '.                  
013900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014100     SKIP2                                                                
014200 01  GODK-STATUSKODER.                                                    
014300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014400     SKIP3                                                                
014500 01  SSA1                        PIC X(64).                               
014600 01  SSA2                        PIC X(64).                               
014700     EJECT                                                                
014800*    --- IMS FUNKTIONSKODER                                               
014900*01  -COPY W0003                                                          
015000     EJECT                                                                
015100*    ---  DLI INPUT-OUTPUT AREA                                           
015200                                                                          
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
015400 01  DLI-IO-WDB601.                                                       
015500*    03  -COPY WDB601                                                     
015600                                                                          
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
015800 01  DLI-IO-WDB615.                                                       
015900*    03  -COPY WDB615                                                     
016000                                                                          
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
016200 01  DLI-IO-WDB616.                                                       
016300*    03  -COPY WDB616                                                     
016400     EJECT                                                                
016500 LINKAGE SECTION.                                                         
016600*01  -COPY W0009   -PRE MSG-                                              
016700*01  -COPY W0008   -PRE WDP7-                                             
016800     05  FILLER                  PIC X.                                   
016900                                                                          
017000*01  -COPY W0008  -PRE WDB6-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
017600                                                                          
017700     PERFORM IMS-GET-MSG                                                  
017800     IF SEGMENT-FINNS                                                     
017900       PERFORM A-INIT                                                     
018000       PERFORM B-KOLLA-NYCKLAR                                            
018100       IF NYCKLAR-OK                                                      
018200         IF MFS-UPDATE                                                    
018300           PERFORM G-KOLLA-INPUT                                          
018400           IF INDATA-OK                                                   
018500             PERFORM H-UPPDATERA                                          
018600           END-IF                                                         
018700         ELSE                                                             
018800           IF MFS-FIRST                                                   
018900             PERFORM C-FOERSTA-SIDA                                       
019000           ELSE                                                           
019100             PERFORM E-SAMMA-SIDA                                         
019200           END-IF                                                         
019300         END-IF                                                           
019400         PERFORM F-LAES-VISA-INFO                                         
019500       END-IF                                                             
019600       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40801 + 4                      
019700       PERFORM IMS-INSERT-MSG                                             
019800     END-IF                                                               
019900                                                                          
020000     MOVE ZERO TO RETURN-CODE                                             
020100     GOBACK                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 A-INIT SECTION.                                                          
020500                                                                          
020600     IF MSG-DUBBLA-TRANSKODER                                             
020700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I40801                 
020800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021000     ELSE                                                                 
021100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I40801                  
021200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021400     END-IF                                                               
021500                                                                          
021600     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
021700     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
021800     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
021900                                                                          
022000     MOVE LOW-VALUE        TO MSG-AREA                                    
022100     MOVE 'W4O408N1'       TO MFS-IDMOD                                   
022200     MOVE '4408'           TO MOD-IDTRANS                                 
022300     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
022400                                                                          
022500     IF EGEN-MID OR HELP-MID                                              
022600       CONTINUE                                                           
022700     ELSE                                                                 
022800       MOVE SPACE TO MFS-KDTRTYP                                          
022900       MOVE '7' TO MFS-IDPFK                                              
023000     END-IF                                                               
023100                                                                          
023200     ACCEPT WS-DATUM         FROM DATE                                    
023300                                                                          
023400     MOVE SPACE            TO MED-IDMFSFEL                                
023500     .                                                                    
023600     EJECT                                                                
023700 B-KOLLA-NYCKLAR SECTION.                                                 
023800                                                                          
023900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024000     MOVE '001'             TO MSGI-KDCALL                                
024100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024300     MOVE '4408'            TO MSGI-IDTRANS                               
024400     IF EGEN-MID                                                          
024500         MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                            
024600     END-IF                                                               
024700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
024800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
024900                                                                          
025000*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
025100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
025200                                                                          
025300     MOVE JA TO NYCKLAR-SW                                                
025400                                                                          
025500                                                                          
025600*    -- KONTROLL AV IDDC                                                  
025700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
025800                                                                          
025900     IF MID-IDDC-IN NOT = ALL '+'                                         
026000       MOVE '7'         TO MFS-IDPFK                                      
026100       MOVE SPACE       TO MFS-KDTRTYP                                    
026200     END-IF                                                               
026300     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
026400                           MOD-IDDC-UT                                    
026500     PERFORM IMS-GU-WDB601                                                
026600                                                                          
026700     IF SEGMENT-SAKNAS                                                    
026800       MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                                
026900       CALL WMEDKONV USING MED-WMEDAREA                                   
027000       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
027100       PERFORM MFS-RENSA-FAELT-UT                                         
027200       MOVE NEJ            TO NYCKLAR-SW                                  
027300     END-IF                                                               
027400                                                                          
027500*    -- KONTROLL AV IDDC-REF                                              
027600     MOVE MFS-RENSA-FAELT TO MOD-IDDC-REF-IN                              
027700                                                                          
027800     IF MID-IDDC-REF-IN NOT = ALL '+'                                     
027900       MOVE '7'         TO MFS-IDPFK                                      
028000       MOVE SPACE       TO MFS-KDTRTYP                                    
028100       MOVE MID-IDDC-REF-IN                                               
028200                        TO SPAR-IDDC-REF                                  
028300                           WS-IDDC                                        
028400     END-IF                                                               
028500                                                                          
028600     MOVE SPAR-IDDC-REF TO W-IDDC                                         
028700                           W-IDDC-REF                                     
028800                           MOD-IDDC-REF-UT                                
028900     PERFORM IMS-GU-WDB601                                                
029000                                                                          
029100     IF SEGMENT-SAKNAS                                                    
029200       MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                                
029300       CALL WMEDKONV USING MED-WMEDAREA                                   
029400       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
029500       PERFORM MFS-RENSA-FAELT-UT                                         
029600       MOVE NEJ            TO NYCKLAR-SW                                  
029700     ELSE                                                                 
029800       IF MSGI-IDDC-KEY = SPAR-IDDC-REF                                   
029900         MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                              
030000         CALL WMEDKONV USING MED-WMEDAREA                                 
030100         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
030200         PERFORM MFS-RENSA-FAELT-UT                                       
030300         MOVE NEJ            TO NYCKLAR-SW                                
030400       END-IF                                                             
030500     END-IF                                                               
030600                                                                          
030700     MOVE '002'             TO MSGI-KDCALL                                
030800     MOVE '4408'            TO MSGI-IDTRANS                               
030900     MOVE SPAR-AREA         TO MSGI-SPAR-AREA                             
031000     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
032600     .                                                                    
032700     EJECT                                                                
032800 C-FOERSTA-SIDA SECTION.                                                  
032900                                                                          
033000     PERFORM MFS-RENSA-FAELT-IN                                           
033100     .                                                                    
033200     EJECT                                                                
033300 E-SAMMA-SIDA SECTION.                                                    
033400                                                                          
033500     IF EGEN-MID OR HELP-MID                                              
033600       IF MID-INPUT = ALL '+'                                             
033700         PERFORM MFS-RENSA-FAELT-IN                                       
033800       ELSE                                                               
033900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
034000         CALL WMEDKONV USING MED-WMEDAREA                                 
034100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
034200         PERFORM EA-MID-INDATA-TILL-MOD                                   
034300         PERFORM EB-KOLLA-NYTT-PA-HYLLAN-DATUM                            
034400       END-IF                                                             
034500     ELSE                                                                 
034600       PERFORM MFS-RENSA-FAELT-IN                                         
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 EA-MID-INDATA-TILL-MOD SECTION.                                          
035100                                                                          
035200     IF MID-KVDLTID-AIRPAC-IN = ALL '+'                                   
035300       MOVE MFS-RENSA-FAELT       TO MOD-KVDLTID-AIRPAC-IN                
035400     ELSE                                                                 
035500       MOVE MID-KVDLTID-AIRPAC-IN TO MOD-KVDLTID-AIRPAC-IN                
035600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDLTID-AIRPAC-IN-ATTR           
035700     END-IF                                                               
035800                                                                          
035900     IF MID-KVDLTID-BOATPAC-IN = ALL '+'                                  
036000       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-BOATPAC-IN              
036100     ELSE                                                                 
036200       MOVE MID-KVDLTID-BOATPAC-IN TO MOD-KVDLTID-BOATPAC-IN              
036300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-BOATPAC-IN-ATTR         
036400     END-IF                                                               
036500                                                                          
036600     IF MID-KVDLTID-AIRTRP-IN = ALL '+'                                   
036700       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-AIRTRP-IN               
036800     ELSE                                                                 
036900       MOVE MID-KVDLTID-AIRTRP-IN  TO MOD-KVDLTID-AIRTRP-IN               
037000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-AIRTRP-IN-ATTR          
037100     END-IF                                                               
037200                                                                          
037300     IF MID-KVDLTID-BOATTRP-IN = ALL '+'                                  
037400       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-BOATTRP-IN              
037500     ELSE                                                                 
037600       MOVE MID-KVDLTID-BOATTRP-IN TO MOD-KVDLTID-BOATTRP-IN              
037700       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-BOATTRP-IN-ATTR         
037800     END-IF                                                               
037900                                                                          
038000     IF MID-KVDLTID-BOAT2DC-IN = ALL '+'                                  
038100       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-BOAT2DC-IN              
038200     ELSE                                                                 
038300       MOVE MID-KVDLTID-BOAT2DC-IN TO MOD-KVDLTID-BOAT2DC-IN              
038400       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-BOAT2DC-IN-ATTR         
038500     END-IF                                                               
038600                                                                          
038700     IF MID-KVDLTID-AIRINS-IN  = ALL '+'                                  
038800       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-AIRINS-IN               
038900     ELSE                                                                 
039000       MOVE MID-KVDLTID-AIRINS-IN  TO MOD-KVDLTID-AIRINS-IN               
039100       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-AIRINS-IN-ATTR          
039200     END-IF                                                               
039300                                                                          
039400     IF MID-KVDLTID-BOATINS-IN = ALL '+'                                  
039500       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-BOATINS-IN              
039600     ELSE                                                                 
039700       MOVE MID-KVDLTID-BOATINS-IN TO MOD-KVDLTID-BOATINS-IN              
039800       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-BOATINS-IN-ATTR         
039900     END-IF                                                               
040000                                                                          
040100     IF MID-KVDLTID-BUFF-IN = ALL '+'                                     
040200       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-BUFF-IN                 
040300     ELSE                                                                 
040400       MOVE MID-KVDLTID-BUFF-IN    TO MOD-KVDLTID-BUFF-IN                 
040500       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-BUFF-IN-ATTR            
040600     END-IF                                                               
040700                                                                          
040800     IF MID-KVDLTID-CUST-IN    = ALL '+'                                  
040900       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-CUST-IN                 
041000     ELSE                                                                 
041100       MOVE MID-KVDLTID-CUST-IN    TO MOD-KVDLTID-CUST-IN                 
041200       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-CUST-IN-ATTR            
041300     END-IF                                                               
041400                                                                          
041500     IF MID-KVDLTID-CUSTWAIT-IN = ALL '+'                                 
041600       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-CUSTWAIT-IN             
041700     ELSE                                                                 
041800       MOVE MID-KVDLTID-CUSTWAIT-IN TO MOD-KVDLTID-CUSTWAIT-IN            
041900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDLTID-CUSTWAIT-IN-ATTR         
042000     END-IF                                                               
042100                                                                          
042200     IF MID-KVDLTID-CUST2DC-IN = ALL '+'                                  
042300       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-CUST2DC-IN              
042400     ELSE                                                                 
042500       MOVE MID-KVDLTID-CUST2DC-IN TO MOD-KVDLTID-CUST2DC-IN              
042600       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-CUST2DC-IN-ATTR         
042700     END-IF                                                               
042800                                                                          
042900     IF MID-KVDLTID-AIRETA-IN = ALL '+'                                   
043000       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-AIRETA-IN               
043100     ELSE                                                                 
043200       MOVE MID-KVDLTID-AIRETA-IN  TO MOD-KVDLTID-AIRETA-IN               
043300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-AIRETA-IN-ATTR          
043400     END-IF                                                               
043500                                                                          
043600     IF MID-KVDLTID-TOT-IN = ALL '+'                                      
043700       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-TOT-IN                  
043800     ELSE                                                                 
043900       MOVE MID-KVDLTID-TOT-IN     TO MOD-KVDLTID-TOT-IN                  
044000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-TOT-IN-ATTR             
044100     END-IF                                                               
044200                                                                          
044300     IF MID-TIREFBAT-IN = ALL '+'                                         
044400       MOVE MFS-RENSA-FAELT        TO MOD-TIREFBAT-IN                     
044500     ELSE                                                                 
044600       MOVE MID-TIREFBAT-IN        TO MOD-TIREFBAT-IN                     
044700       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TIREFBAT-IN-ATTR                
044800     END-IF                                                               
044900                                                                          
045000     IF MID-REAIRCO-IN  = ALL '+'                                         
045100       MOVE MFS-RENSA-FAELT        TO MOD-REAIRCO-IN                      
045200     ELSE                                                                 
081402       INSPECT MID-REAIRCO-IN                                             
081403               REPLACING LEADING SPACE BY ZERO                            
             MOVE MID-REAIRCO-IN           TO DEC-IDFRIDATA                     
             MOVE 5                        TO DEC-KVHELTAL                      
             MOVE 1                        TO DEC-KVDECIMAL                     
             CALL WDECEDIT USING DEC-WDECAREA                                   
081701       IF DEC-KDSVAR-OK                                                   
               MOVE DEC-IDEDITDATA         TO W-REAIRCO                         
043400         MOVE W-REAIRCO              TO MOD-REAIRCO-IN                    
045400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-REAIRCO-IN-ATTR               
083001       ELSE                                                               
083101         MOVE MFS-NUM-FAELT-FEL      TO MOD-REAIRCO-IN-ATTR               
083301       END-IF                                                             
045500     END-IF                                                               
045600                                                                          
045700     IF MID-KVDLTID-AIRREQ-IN = ALL '+'                                   
045800       MOVE MFS-RENSA-FAELT        TO MOD-KVDLTID-AIRREQ-IN               
045900     ELSE                                                                 
046000       MOVE MID-KVDLTID-AIRREQ-IN  TO MOD-KVDLTID-AIRREQ-IN               
046100       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDLTID-AIRREQ-IN-ATTR          
046200     END-IF                                                               
046400                                                                          
045000     IF MID-RESSFAC-IN  = ALL '+'                                         
045100       MOVE MFS-RENSA-FAELT        TO MOD-RESSFAC-IN                      
045200     ELSE                                                                 
081402       INSPECT MID-RESSFAC-IN                                             
081403               REPLACING LEADING SPACE BY ZERO                            
081403       MOVE MID-RESSFAC-IN           TO DEC-IDFRIDATA                     
081403       MOVE 1                        TO DEC-KVHELTAL                      
081403       MOVE 2                        TO DEC-KVDECIMAL                     
081403       CALL WDECEDIT USING DEC-WDECAREA                                   
081701       IF DEC-KDSVAR-OK                                                   
043400         MOVE DEC-IDEDITDATA         TO W-RESSFAC                         
043400         MOVE W-RESSFAC              TO MOD-RESSFAC-IN                    
045400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-RESSFAC-IN-ATTR               
083001       ELSE                                                               
083101         MOVE MFS-NUM-FAELT-FEL      TO MOD-RESSFAC-IN-ATTR               
083301       END-IF                                                             
045500     END-IF                                                               
045600                                                                          
044300     IF MID-PRFRAKT-IN = ALL '+'                                          
044400       MOVE MFS-RENSA-FAELT        TO MOD-PRFRAKT-IN                      
044500     ELSE                                                                 
081402       INSPECT MID-PRFRAKT-IN                                             
081403               REPLACING LEADING SPACE BY ZERO                            
081403       MOVE MID-PRFRAKT-IN           TO DEC-IDFRIDATA                     
081403       MOVE 7                        TO DEC-KVHELTAL                      
081403       MOVE 0                        TO DEC-KVDECIMAL                     
081403       CALL WDECEDIT USING DEC-WDECAREA                                   
081701       IF DEC-KDSVAR-OK                                                   
043400         MOVE DEC-IDEDITDATA         TO W-PRFRAKT                         
043400         MOVE W-PRFRAKT              TO MOD-PRFRAKT-IN                    
045400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-PRFRAKT-IN-ATTR               
083001       ELSE                                                               
083101         MOVE MFS-NUM-FAELT-FEL      TO MOD-PRFRAKT-IN-ATTR               
083301       END-IF                                                             
045500     END-IF                                                               
046300                                                                          
046400     MOVE MID-BETEXT               TO MOD-BETEXT                          
046500     .                                                                    
046600     EJECT                                                                
046700 EB-KOLLA-NYTT-PA-HYLLAN-DATUM SECTION.                                   
046800                                                                          
046900     MOVE MSGI-IDDC-KEY  TO W-IDDC                                        
047000     MOVE SPAR-IDDC-REF  TO W-IDDC-REF                                    
047100     PERFORM IMS-GU-WDB616                                                
047200     IF SEGMENT-SAKNAS                                                    
047300        MOVE +1          TO REF-KVDLTID-BOATPAC                           
047400        MOVE ZERO        TO REF-KVDLTID-AIRPAC                            
047500                            REF-KVDLTID-BOAT2DC                           
047600                            REF-KVDLTID-AIRINS                            
047700                            REF-KVDLTID-BOATINS                           
047800                            REF-KVDLTID-AIRTRP                            
047900                            REF-KVDLTID-BOATTRP                           
048000                            REF-KVDLTID-BUFF                              
048100     END-IF                                                               
048200     IF MID-KVDLTID-AIRPAC-IN NOT = ALL '+'                               
048300       MOVE MID-KVDLTID-AIRPAC-IN  TO REF-KVDLTID-AIRPAC                  
048400     END-IF                                                               
048500                                                                          
048600     IF MID-KVDLTID-AIRTRP-IN NOT = ALL '+'                               
048700       MOVE MID-KVDLTID-AIRTRP-IN  TO REF-KVDLTID-AIRTRP                  
048800     END-IF                                                               
048900                                                                          
049000     IF MID-KVDLTID-AIRINS-IN NOT = ALL '+'                               
049100       MOVE MID-KVDLTID-AIRINS-IN  TO REF-KVDLTID-AIRINS                  
049200     END-IF                                                               
049300                                                                          
049400     IF MID-KVDLTID-BOATPAC-IN NOT = ALL '+'                              
049500       MOVE MID-KVDLTID-BOATPAC-IN TO REF-KVDLTID-BOATPAC                 
049600     END-IF                                                               
049700                                                                          
049800     IF MID-KVDLTID-BOATTRP-IN NOT = ALL '+'                              
049900       MOVE MID-KVDLTID-BOATTRP-IN TO REF-KVDLTID-BOATTRP                 
050000     END-IF                                                               
050100                                                                          
050200     IF MID-KVDLTID-BOAT2DC-IN NOT = ALL '+'                              
050300       MOVE MID-KVDLTID-BOAT2DC-IN TO REF-KVDLTID-BOAT2DC                 
050400     END-IF                                                               
050500                                                                          
050600     IF MID-KVDLTID-BOATINS-IN NOT = ALL '+'                              
050700       MOVE MID-KVDLTID-BOATINS-IN TO MOD-KVDLTID-BOATINS-IN              
050800     END-IF                                                               
050900                                                                          
051000     IF MID-KVDLTID-BUFF-IN NOT = ALL '+'                                 
051100       MOVE MID-KVDLTID-BUFF-IN    TO MOD-KVDLTID-BUFF-IN                 
051200     END-IF                                                               
051300                                                                          
051400     IF MID-KVDLTID-AIRPAC-IN NOT = ALL '+' OR                            
051500        MID-KVDLTID-AIRTRP-IN NOT = ALL '+' OR                            
051600        MID-KVDLTID-AIRINS-IN NOT = ALL '+'                               
051700                                                                          
051800        PERFORM S01-FLYG-PA-HYLLAN                                        
051900     END-IF                                                               
052000                                                                          
052100     IF MID-KVDLTID-BOATPAC-IN NOT = ALL '+' OR                           
052200        MID-KVDLTID-BOATTRP-IN NOT = ALL '+' OR                           
052300        MID-KVDLTID-BOAT2DC-IN NOT = ALL '+' OR                           
052400        MID-KVDLTID-BOATINS-IN NOT = ALL '+' OR                           
052500        MID-KVDLTID-BUFF-IN    NOT = ALL '+'                              
052600                                                                          
052700        PERFORM S02-BAAT-PA-HYLLAN                                        
052800     END-IF                                                               
052900     .                                                                    
053000     EJECT                                                                
053100 F-LAES-VISA-INFO SECTION.                                                
053200                                                                          
053300     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
053400     MOVE '4408'             TO W-IDTRANS-B6                              
053500     MOVE SPAR-IDDC-REF      TO W-IDDC-REF-B6                             
053600     PERFORM IMS-GU-WDB615                                                
053700     IF SEGMENT-FINNS                                                     
053800        MOVE LOGG-IDUSER     TO  MOD-IDUSER                               
053900        MOVE LOGG-TIUPPDAT   TO  MOD-TIUPPDAT                             
054000     ELSE                                                                 
054100        MOVE MFS-RENSA-FAELT TO MOD-IDUSER                                
054200                                MOD-TIUPPDAT                              
054300     END-IF                                                               
054400                                                                          
054500     MOVE JA                      TO REF-SEG-SW                           
054600     MOVE SPAR-IDDC-REF TO W-IDDC-REF                                     
054700     PERFORM IMS-GU-WDB616                                                
054800                                                                          
054900     IF SEGMENT-FINNS                                                     
055000        MOVE REF-KVDLTID-AIRPAC    TO MOD-KVDLTID-AIRPAC                  
055100        MOVE REF-KVDLTID-BOATPAC   TO MOD-KVDLTID-BOATPAC                 
055200        MOVE REF-KVDLTID-AIRTRP    TO MOD-KVDLTID-AIRTRP                  
055300        MOVE REF-KVDLTID-BOATTRP   TO MOD-KVDLTID-BOATTRP                 
055400        MOVE REF-KVDLTID-BOAT2DC   TO MOD-KVDLTID-BOAT2DC                 
055500        MOVE REF-KVDLTID-AIRINS    TO MOD-KVDLTID-AIRINS                  
055600        MOVE REF-KVDLTID-BOATINS   TO MOD-KVDLTID-BOATINS                 
055700        MOVE REF-KVDLTID-BUFF      TO MOD-KVDLTID-BUFF                    
055800        MOVE REF-KVDLTID-CUST      TO MOD-KVDLTID-CUST                    
055900        MOVE REF-KVDLTID-CUSTWAIT  TO MOD-KVDLTID-CUSTWAIT                
056000        MOVE REF-KVDLTID-CUST2DC   TO MOD-KVDLTID-CUST2DC                 
056100        MOVE REF-KVDLTID-AIRETA    TO MOD-KVDLTID-AIRETA                  
056200        MOVE REF-KVDLTID-TOT       TO MOD-KVDLTID-TOT                     
056300        MOVE REF-KVDLTID-AIRREQ    TO MOD-KVDLTID-AIRREQ                  
056400        MOVE REF-TIREFBAT          TO MOD-TIREFBAT                        
056500        MOVE REF-REAIRCO           TO MOD-REAIRCO                         
056500        MOVE REF-RESSFAC           TO MOD-RESSFAC                         
056600        MOVE REF-PRFRAKT           TO MOD-PRFRAKT                         
056600        MOVE REF-BETEXT            TO MOD-BETEXT                          
056700     ELSE                                                                 
056800        MOVE NEJ                   TO REF-SEG-SW                          
056900        MOVE ZERO                  TO MOD-KVDLTID-AIRPAC-IN               
057000                                      REF-KVDLTID-AIRPAC                  
057100                                      MOD-KVDLTID-AIRTRP-IN               
057200                                      REF-KVDLTID-AIRTRP                  
057300                                      MOD-KVDLTID-BOATTRP-IN              
057400                                      REF-KVDLTID-BOATTRP                 
057500                                      MOD-KVDLTID-BOAT2DC-IN              
057600                                      REF-KVDLTID-BOAT2DC                 
057700                                      MOD-KVDLTID-AIRINS-IN               
057800                                      REF-KVDLTID-AIRINS                  
057900                                      MOD-KVDLTID-BOATINS-IN              
058000                                      REF-KVDLTID-BOATINS                 
058100                                      MOD-KVDLTID-BUFF-IN                 
058200                                      REF-KVDLTID-BUFF                    
058300                                      MOD-KVDLTID-CUST-IN                 
058400                                      REF-KVDLTID-CUST                    
058500                                      MOD-KVDLTID-CUSTWAIT-IN             
058600                                      REF-KVDLTID-CUSTWAIT                
058700                                      MOD-KVDLTID-CUST2DC-IN              
058800                                      REF-KVDLTID-CUST2DC                 
058900                                      MOD-KVDLTID-AIRETA-IN               
059000                                      REF-KVDLTID-AIRETA                  
059100                                      MOD-TIREFBAT-IN                     
059200                                      REF-TIREFBAT                        
059300                                      MOD-REAIRCO-IN                      
059400                                      REF-REAIRCO                         
060200                                                                          
059600        MOVE +1                    TO MOD-KVDLTID-BOATPAC-IN              
059700                                      REF-KVDLTID-BOATPAC                 
059800                                      MOD-KVDLTID-TOT-IN                  
059900                                      REF-KVDLTID-TOT                     
060000                                      MOD-KVDLTID-AIRREQ-IN               
060100                                      REF-KVDLTID-AIRREQ                  
060200                                                                          
059300        MOVE +1.00                 TO MOD-RESSFAC-IN                      
059400                                      REF-RESSFAC                         
060200                                                                          
059300        MOVE +10000                TO MOD-PRFRAKT-IN                      
059400                                      REF-PRFRAKT                         
059500                                                                          
060300        MOVE INF-PRESS-PF11        TO MED-IDMFSINF                        
060400        CALL WMEDKONV USING MED-WMEDAREA                                  
060500        MOVE MED-MFSINF            TO MOD-TEMFSINF                        
060600        MOVE MFS-RENSA-FAELT       TO MOD-BETEXT                          
060700        MOVE SPACE                 TO REF-BETEXT                          
060800     END-IF                                                               
060900                                                                          
061000     PERFORM S01-FLYG-PA-HYLLAN                                           
061100     PERFORM S02-BAAT-PA-HYLLAN                                           
061200     PERFORM S03-BUFF-PA-HYLLAN                                           
061300     .                                                                    
061400     EJECT                                                                
061500 G-KOLLA-INPUT SECTION.                                                   
061600                                                                          
061700     MOVE JA  TO INDATA-SW                                                
061800                                                                          
061900     MOVE MSGI-IDDC-KEY      TO W-IDDC                                    
062000     MOVE SPAR-IDDC-REF      TO W-IDDC-REF-B6                             
062100     PERFORM IMS-GU-WDB616                                                
062200     IF  MID-INPUT     = ALL '+'                                          
062300     AND SEGMENT-FINNS                                                    
062400     AND (      MID-BETEXT  = REF-BETEXT                                  
062500         OR (   MID-BETEXT  = ALL '+'                                     
062600            AND REF-BETEXT  = SPACE  )                                    
062700         )                                                                
062800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
062800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
062900       CALL WMEDKONV USING MED-WMEDAREA                                   
063000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
063100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
063200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
063300       MOVE NEJ TO INDATA-SW                                              
063400     ELSE                                                                 
063500                                                                          
063600       PERFORM GA-KOLLA-ANDR-SEGM                                         
063700                                                                          
063800       IF INDATA-FEL                                                      
063900         IF MED-IDMFSFEL = SPACE                                          
064000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
064100         END-IF                                                           
064200         CALL WMEDKONV USING MED-WMEDAREA                                 
064300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
064400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
064500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
064600       END-IF                                                             
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 GA-KOLLA-ANDR-SEGM SECTION.                                              
065100                                                                          
065200     IF MID-KVDLTID-AIRPAC-IN NOT = ALL '+'                               
065300       INSPECT MID-KVDLTID-AIRPAC-IN                                      
065400               REPLACING LEADING SPACE BY ZERO                            
065500       IF MID-KVDLTID-AIRPAC-IN NUMERIC                                   
065600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-AIRPAC-IN-ATTR           
065700       ELSE                                                               
065800         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-AIRPAC-IN-ATTR             
065900         MOVE NEJ TO INDATA-SW                                            
066000       END-IF                                                             
066100     END-IF                                                               
066200                                                                          
066300     IF MID-KVDLTID-BOATPAC-IN NOT = ALL '+'                              
066400       INSPECT MID-KVDLTID-BOATPAC-IN                                     
066500               REPLACING LEADING SPACE BY ZERO                            
066600       IF MID-KVDLTID-BOATPAC-IN NUMERIC                                  
066700         IF MID-KVDLTID-BOATPAC-IN = ZERO                                 
066800           MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-BOATPAC-IN-ATTR          
066900           MOVE NEJ TO INDATA-SW                                          
067000         ELSE                                                             
067100           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-BOATPAC-IN-ATTR        
067200         END-IF                                                           
067300       ELSE                                                               
067400         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-BOATPAC-IN-ATTR            
067500         MOVE NEJ TO INDATA-SW                                            
067600       END-IF                                                             
067700     END-IF                                                               
067800                                                                          
067900     IF MID-KVDLTID-AIRTRP-IN NOT = ALL '+'                               
068000       INSPECT MID-KVDLTID-AIRTRP-IN                                      
068100               REPLACING LEADING SPACE BY ZERO                            
068200       IF MID-KVDLTID-AIRTRP-IN NUMERIC                                   
068300         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-AIRTRP-IN-ATTR           
068400       ELSE                                                               
068500         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-AIRTRP-IN-ATTR             
068600         MOVE NEJ TO INDATA-SW                                            
068700       END-IF                                                             
068800     END-IF                                                               
068900                                                                          
069000     IF MID-KVDLTID-BOATTRP-IN NOT = ALL '+'                              
069100       INSPECT MID-KVDLTID-BOATTRP-IN                                     
069200               REPLACING LEADING SPACE BY ZERO                            
069300       IF MID-KVDLTID-BOATTRP-IN NUMERIC                                  
069400         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-BOATTRP-IN-ATTR          
069500       ELSE                                                               
069600         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-BOATTRP-IN-ATTR            
069700         MOVE NEJ TO INDATA-SW                                            
069800       END-IF                                                             
069900     END-IF                                                               
070000                                                                          
070100     IF MID-KVDLTID-BOAT2DC-IN NOT = ALL '+'                              
070200       INSPECT MID-KVDLTID-BOAT2DC-IN                                     
070300               REPLACING LEADING SPACE BY ZERO                            
070400       IF MID-KVDLTID-BOAT2DC-IN NUMERIC                                  
070500         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-BOAT2DC-IN-ATTR          
070600       ELSE                                                               
070700         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-BOAT2DC-IN-ATTR            
070800         MOVE NEJ TO INDATA-SW                                            
070900       END-IF                                                             
071000     END-IF                                                               
071100                                                                          
071200     IF MID-KVDLTID-AIRINS-IN NOT = ALL '+'                               
071300       INSPECT MID-KVDLTID-AIRINS-IN                                      
071400               REPLACING LEADING SPACE BY ZERO                            
071500       IF MID-KVDLTID-AIRINS-IN NUMERIC                                   
071600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-AIRINS-IN-ATTR           
071700       ELSE                                                               
071800         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-AIRINS-IN-ATTR             
071900         MOVE NEJ TO INDATA-SW                                            
072000       END-IF                                                             
072100     END-IF                                                               
072200                                                                          
072300     IF MID-KVDLTID-BOATINS-IN NOT = ALL '+'                              
072400       INSPECT MID-KVDLTID-BOATINS-IN                                     
072500               REPLACING LEADING SPACE BY ZERO                            
072600       IF MID-KVDLTID-BOATINS-IN NUMERIC                                  
072700         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-BOATINS-IN-ATTR          
072800       ELSE                                                               
072900         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-BOATINS-IN-ATTR            
073000         MOVE NEJ TO INDATA-SW                                            
073100       END-IF                                                             
073200     END-IF                                                               
073300                                                                          
073400     IF MID-KVDLTID-BUFF-IN NOT = ALL '+'                                 
073500       INSPECT MID-KVDLTID-BUFF-IN                                        
073600               REPLACING LEADING SPACE BY ZERO                            
073700       IF MID-KVDLTID-BUFF-IN NUMERIC                                     
073800         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-BUFF-IN-ATTR             
073900       ELSE                                                               
074000         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-BUFF-IN-ATTR               
074100         MOVE NEJ TO INDATA-SW                                            
074200       END-IF                                                             
074300     END-IF                                                               
074400                                                                          
074500     IF MID-KVDLTID-CUST-IN NOT = ALL '+'                                 
074600       INSPECT MID-KVDLTID-CUST-IN                                        
074700               REPLACING LEADING SPACE BY ZERO                            
074800       IF MID-KVDLTID-CUST-IN NUMERIC                                     
074900         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-CUST-IN-ATTR             
075000       ELSE                                                               
075100         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-CUST-IN-ATTR               
075200         MOVE NEJ TO INDATA-SW                                            
075300       END-IF                                                             
075400     END-IF                                                               
075500                                                                          
075600     IF MID-KVDLTID-CUSTWAIT-IN NOT = ALL '+'                             
075700       INSPECT MID-KVDLTID-CUSTWAIT-IN                                    
075800               REPLACING LEADING SPACE BY ZERO                            
075900       IF MID-KVDLTID-CUSTWAIT-IN NUMERIC                                 
076000         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-CUSTWAIT-IN-ATTR         
076100       ELSE                                                               
076200         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-CUSTWAIT-IN-ATTR           
076300         MOVE NEJ TO INDATA-SW                                            
076400       END-IF                                                             
076500     END-IF                                                               
076600                                                                          
076700     IF MID-KVDLTID-CUST2DC-IN NOT = ALL '+'                              
076800       INSPECT MID-KVDLTID-CUST2DC-IN                                     
076900               REPLACING LEADING SPACE BY ZERO                            
077000       IF MID-KVDLTID-CUST2DC-IN NUMERIC                                  
077100         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-CUST2DC-IN-ATTR          
077200       ELSE                                                               
077300         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-CUST2DC-IN-ATTR            
077400         MOVE NEJ TO INDATA-SW                                            
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800     IF MID-KVDLTID-AIRETA-IN NOT = ALL '+'                               
077900       INSPECT MID-KVDLTID-AIRETA-IN                                      
078000               REPLACING LEADING SPACE BY ZERO                            
078100       IF MID-KVDLTID-AIRETA-IN NUMERIC                                   
078200         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-AIRETA-IN-ATTR           
078300       ELSE                                                               
078400         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-AIRETA-IN-ATTR             
078500         MOVE NEJ TO INDATA-SW                                            
078600       END-IF                                                             
078700     END-IF                                                               
078800                                                                          
078900     IF MID-KVDLTID-TOT-IN NOT = ALL '+'                                  
079000       INSPECT MID-KVDLTID-TOT-IN                                         
079100               REPLACING LEADING SPACE BY ZERO                            
079200       IF MID-KVDLTID-TOT-IN NUMERIC                                      
079300         IF MID-KVDLTID-TOT-IN = ZERO                                     
079400           MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-TOT-IN-ATTR              
079500           MOVE NEJ TO INDATA-SW                                          
079600         ELSE                                                             
079700           IF MID-KVDLTID-TOT-IN > MAX-ALLOW-LT                           
079800              MOVE MED-1               TO MOD-TEMFSINF                    
079900              MOVE MFS-NUM-FAELT-FEL   TO MOD-KVDLTID-TOT-IN-ATTR         
080000              MOVE NEJ                 TO INDATA-SW                       
080100           ELSE                                                           
080200              MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-TOT-IN-ATTR         
080300           END-IF                                                         
080400         END-IF                                                           
080500       ELSE                                                               
080600         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-TOT-IN-ATTR                
080700         MOVE NEJ TO INDATA-SW                                            
080800       END-IF                                                             
080900     END-IF                                                               
081000                                                                          
081100     IF MID-TIREFBAT-IN NOT = ALL '+'                                     
081200       INSPECT MID-TIREFBAT-IN                                            
081300               REPLACING LEADING SPACE BY ZERO                            
081400       IF MID-TIREFBAT-IN NUMERIC                                         
081500         IF MID-TIREFBAT-IN = ZERO OR 2 OR 5                              
081600           IF MID-TIREFBAT-IN = 5                                         
081700           AND W-IDDC = '11'                                              
081800             MOVE MFS-NUM-FAELT-FEL TO MOD-TIREFBAT-IN-ATTR               
081900             MOVE NEJ TO INDATA-SW                                        
082000           ELSE                                                           
082100             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIREFBAT-IN-ATTR             
082200           END-IF                                                         
082300         ELSE                                                             
082400           MOVE MFS-NUM-FAELT-FEL TO MOD-TIREFBAT-IN-ATTR                 
082500           MOVE NEJ TO INDATA-SW                                          
082600         END-IF                                                           
082700       ELSE                                                               
082800         MOVE MFS-NUM-FAELT-FEL TO MOD-TIREFBAT-IN-ATTR                   
082900         MOVE NEJ TO INDATA-SW                                            
083000       END-IF                                                             
083100     END-IF                                                               
083200                                                                          
083300     IF MID-REAIRCO-IN NOT = ALL '+'                                      
081402       INSPECT MID-REAIRCO-IN                                             
081403               REPLACING LEADING SPACE BY ZERO                            
             MOVE MID-REAIRCO-IN           TO DEC-IDFRIDATA                     
             MOVE 5                        TO DEC-KVHELTAL                      
             MOVE 1                        TO DEC-KVDECIMAL                     
             CALL WDECEDIT USING DEC-WDECAREA                                   
081701       IF DEC-KDSVAR-OK                                                   
               MOVE DEC-IDEDITDATA    TO W-REAIRCO                              
077800         IF W-REAIRCO          >= MAX-REAIRCO                             
077900            MOVE MED-2               TO MOD-TEMFSINF                      
078000            MOVE MFS-NUM-FAELT-FEL   TO MOD-REAIRCO-IN-ATTR               
078100            MOVE NEJ                 TO INDATA-SW                         
               ELSE                                                             
                  MOVE MFS-NUM-FAELT-RAETT TO MOD-REAIRCO-IN-ATTR               
               END-IF                                                           
083001       ELSE                                                               
083101         MOVE MFS-NUM-FAELT-FEL TO MOD-REAIRCO-IN-ATTR                    
083201         MOVE NEJ TO INDATA-SW                                            
083301       END-IF                                                             
084100     END-IF                                                               
084200                                                                          
084300                                                                          
084400     IF MID-KVDLTID-AIRREQ-IN NOT = ALL '+'                               
084500       INSPECT MID-KVDLTID-AIRREQ-IN                                      
084600               REPLACING LEADING SPACE BY ZERO                            
084700       IF MID-KVDLTID-AIRREQ-IN NUMERIC                                   
084800         IF MID-KVDLTID-AIRREQ-IN = ZERO                                  
084900           MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-AIRREQ-IN-ATTR           
085000           MOVE NEJ TO INDATA-SW                                          
085100         ELSE                                                             
085200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDLTID-AIRREQ-IN-ATTR         
085300         END-IF                                                           
085400       ELSE                                                               
085500         MOVE MFS-NUM-FAELT-FEL TO MOD-KVDLTID-AIRREQ-IN-ATTR             
085600         MOVE NEJ TO INDATA-SW                                            
085700       END-IF                                                             
085800     END-IF                                                               
084200                                                                          
083300     IF MID-RESSFAC-IN NOT = ALL '+'                                      
081402       INSPECT MID-RESSFAC-IN                                             
081403               REPLACING LEADING SPACE BY ZERO                            
081403       MOVE MID-RESSFAC-IN           TO DEC-IDFRIDATA                     
081403       MOVE 1                        TO DEC-KVHELTAL                      
081403       MOVE 2                        TO DEC-KVDECIMAL                     
081403       CALL WDECEDIT USING DEC-WDECAREA                                   
081701       IF DEC-KDSVAR-OK                                                   
081701         MOVE DEC-IDEDITDATA         TO W-RESSFAC                         
077800         IF W-RESSFAC  = ZERO                                             
077900            MOVE MED-3               TO MOD-TEMFSINF                      
078000            MOVE MFS-NUM-FAELT-FEL   TO MOD-RESSFAC-IN-ATTR               
078100            MOVE NEJ                 TO INDATA-SW                         
083001         ELSE                                                             
083001            MOVE MFS-NUM-FAELT-RAETT TO MOD-RESSFAC-IN-ATTR               
083001         END-IF                                                           
083001       ELSE                                                               
077800         IF DEC-IDFRIDATA = MAX-RESSFAC                                   
077900            MOVE MED-4               TO MOD-TEMFSINF                      
083301         END-IF                                                           
083101         MOVE MFS-NUM-FAELT-FEL      TO MOD-RESSFAC-IN-ATTR               
083201         MOVE NEJ                    TO INDATA-SW                         
083301       END-IF                                                             
084100     END-IF                                                               
084200                                                                          
083300     IF MID-PRFRAKT-IN NOT = ALL '+'                                      
081402       INSPECT MID-PRFRAKT-IN                                             
081403               REPLACING LEADING SPACE BY ZERO                            
081403       MOVE MID-PRFRAKT-IN           TO DEC-IDFRIDATA                     
081403       MOVE 7                        TO DEC-KVHELTAL                      
081403       MOVE 0                        TO DEC-KVDECIMAL                     
081403       CALL WDECEDIT USING DEC-WDECAREA                                   
081701       IF DEC-KDSVAR-OK                                                   
081701         MOVE DEC-IDEDITDATA    TO W-PRFRAKT                              
077800         IF W-PRFRAKT           <  ZERO                                   
078000            MOVE MFS-NUM-FAELT-FEL   TO MOD-PRFRAKT-IN-ATTR               
078100            MOVE NEJ                 TO INDATA-SW                         
083001         ELSE                                                             
083001            MOVE MFS-NUM-FAELT-RAETT TO MOD-PRFRAKT-IN-ATTR               
083001         END-IF                                                           
083001       ELSE                                                               
083101         MOVE MFS-NUM-FAELT-FEL TO MOD-PRFRAKT-IN-ATTR                    
083201         MOVE NEJ TO INDATA-SW                                            
083301       END-IF                                                             
084100     END-IF                                                               
085900     .                                                                    
086000     EJECT                                                                
086100 H-UPPDATERA SECTION.                                                     
086200                                                                          
086300     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
086400     MOVE '4408'        TO W-IDTRANS-B6                                   
086500     MOVE SPAR-IDDC-REF TO W-IDDC-REF-B6                                  
086600                                                                          
086700     PERFORM IMS-GHU-WDB615                                               
086800     IF SEGMENT-FINNS AND                                                 
086900        W-IDDC-REF = LOGG-IDDC-REF                                        
087000        MOVE W-IDDC-REF            TO LOGG-IDDC-REF                       
087100        MOVE MSGI-IDUSER           TO LOGG-IDUSER                         
087200        MOVE WS-DATUM              TO LOGG-TIUPPDAT                       
087300        PERFORM IMS-REPL-WDB615                                           
087400     ELSE                                                                 
087500        MOVE '4408'                TO LOGG-IDTRANS                        
087600        MOVE W-IDDC-REF            TO LOGG-IDDC-REF                       
087700        MOVE MSGI-IDUSER           TO LOGG-IDUSER                         
087800        MOVE WS-DATUM              TO LOGG-TIUPPDAT                       
087900        PERFORM IMS-ISRT-WDB615                                           
088000     END-IF                                                               
088100                                                                          
088200     PERFORM IMS-GHU-WDB616                                               
088300     IF SEGMENT-SAKNAS                                                    
088400        MOVE W-IDDC-REF            TO REF-IDDC-REF                        
088500        MOVE ZERO                  TO REF-IDDISTR-REFILL                  
088600                                      REF-IDDISTR-RETUR                   
088700                                      REF-IDDISTR-QRETUR                  
088800                                      REF-IDKUNDNR-BPS                    
088900                                      REF-IDKUNDNR-SBPS                   
089000                                      REF-IDKUNDNR-RETUR                  
089100                                      REF-IDKUNDNR-QRETUR                 
089200                                      REF-IDKUNDNR-SRETUR                 
089300                                      REF-IDKUNDNR-SQRET                  
089400                                      REF-IDKUNDNR-TRETUR                 
089500                                      REF-IDKUNDNR-SORD                   
089600        MOVE ZERO                  TO REF-KVDLTID-BOATTRP                 
089700                                      REF-KVDLTID-BOAT2DC                 
089800                                      REF-KVDLTID-BOATINS                 
089900                                      REF-KVDLTID-AIRETA                  
090000                                      REF-KVDLTID-AIRPAC                  
090100                                      REF-KVDLTID-BUFF                    
090200                                      REF-KVDLTID-AIRTRP                  
090300                                      REF-KVDLTID-AIRINS                  
090400                                      REF-KVDLTID-CUST                    
090500                                      REF-KVDLTID-CUSTWAIT                
090600                                      REF-KVDLTID-CUST2DC                 
090700                                      REF-TIREFBAT                        
090800                                      REF-REAIRCO                         
090900        MOVE +1                    TO REF-KVDLTID-BOATPAC                 
091000                                      REF-KVDLTID-TOT                     
091100                                      REF-KVDLTID-AIRREQ                  
090800        MOVE +1.00                 TO REF-RESSFAC                         
090800        MOVE +10000                TO REF-PRFRAKT                         
091200        MOVE SPACE                 TO REF-BETEXT                          
091200        PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX            
091200           IF INDX = 7                                                    
091200             MOVE 'Y'              TO REF-FLREFDAY (INDX)                 
091200                                      REF-FLREFBLK (INDX)                 
091200                                      REF-KDREFDG  (INDX)                 
091200           ELSE                                                           
091200             MOVE 'N'              TO REF-FLREFDAY (INDX)                 
091200                                      REF-FLREFBLK (INDX)                 
091200             MOVE SPACE            TO REF-KDREFDG  (INDX)                 
091200           END-IF                                                         
091200        END-PERFORM                                                       
091300        PERFORM IMS-ISRT-WDB616                                           
091400        PERFORM IMS-GHU-WDB616                                            
091500     END-IF                                                               
091600                                                                          
091700     IF MID-KVDLTID-AIRPAC-IN NOT = ALL '+'                               
091800       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-AIRPAC-IN-ATTR          
091900       MOVE MID-KVDLTID-AIRPAC-IN  TO REF-KVDLTID-AIRPAC                  
092000     END-IF                                                               
092100                                                                          
092200     IF MID-KVDLTID-BOATPAC-IN NOT = ALL '+'                              
092300       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-BOATPAC-IN-ATTR         
092400       MOVE MID-KVDLTID-BOATPAC-IN TO REF-KVDLTID-BOATPAC                 
092500     END-IF                                                               
092600                                                                          
092700     IF MID-KVDLTID-AIRTRP-IN NOT = ALL '+'                               
092800       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-AIRTRP-IN-ATTR          
092900       MOVE MID-KVDLTID-AIRTRP-IN  TO REF-KVDLTID-AIRTRP                  
093000     END-IF                                                               
093100                                                                          
093200     IF MID-KVDLTID-BOATTRP-IN NOT = ALL '+'                              
093300       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-BOATTRP-IN-ATTR         
093400       MOVE MID-KVDLTID-BOATTRP-IN TO REF-KVDLTID-BOATTRP                 
093500     END-IF                                                               
093600                                                                          
093700     IF MID-KVDLTID-BOAT2DC-IN NOT = ALL '+'                              
093800       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-BOAT2DC-IN-ATTR         
093900       MOVE MID-KVDLTID-BOAT2DC-IN TO REF-KVDLTID-BOAT2DC                 
094000     END-IF                                                               
094100                                                                          
094200     IF MID-KVDLTID-AIRINS-IN NOT = ALL '+'                               
094300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDLTID-AIRINS-IN-ATTR           
094400       MOVE MID-KVDLTID-AIRINS-IN TO REF-KVDLTID-AIRINS                   
094500     END-IF                                                               
094600                                                                          
094700     IF MID-KVDLTID-BOATINS-IN NOT = ALL '+'                              
094800       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-BOATINS-IN-ATTR         
094900       MOVE MID-KVDLTID-BOATINS-IN TO REF-KVDLTID-BOATINS                 
095000     END-IF                                                               
095100                                                                          
095200     IF MID-KVDLTID-BUFF-IN NOT = ALL '+'                                 
095300       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-BUFF-IN-ATTR            
095400       MOVE MID-KVDLTID-BUFF-IN    TO REF-KVDLTID-BUFF                    
095500     END-IF                                                               
095600                                                                          
095700     IF MID-KVDLTID-CUST-IN NOT = ALL '+'                                 
095800       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-CUST-IN-ATTR            
095900       MOVE MID-KVDLTID-CUST-IN    TO REF-KVDLTID-CUST                    
096000     END-IF                                                               
096100                                                                          
096200     IF MID-KVDLTID-CUSTWAIT-IN NOT = ALL '+'                             
096300       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-CUSTWAIT-IN-ATTR        
096400       MOVE MID-KVDLTID-CUSTWAIT-IN TO REF-KVDLTID-CUSTWAIT               
096500     END-IF                                                               
096600                                                                          
096700     IF MID-KVDLTID-CUST2DC-IN NOT = ALL '+'                              
096800       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-CUST2DC-IN-ATTR         
096900       MOVE MID-KVDLTID-CUST2DC-IN TO REF-KVDLTID-CUST2DC                 
097000     END-IF                                                               
097100                                                                          
097200     IF MID-KVDLTID-AIRETA-IN NOT = ALL '+'                               
097300       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-AIRETA-IN-ATTR          
097400       MOVE MID-KVDLTID-AIRETA-IN  TO REF-KVDLTID-AIRETA                  
097500     END-IF                                                               
097600                                                                          
097700     IF MID-KVDLTID-TOT-IN NOT = ALL '+'                                  
097800       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVDLTID-TOT-IN-ATTR             
097900       MOVE MID-KVDLTID-TOT-IN     TO REF-KVDLTID-TOT                     
098000     END-IF                                                               
098100                                                                          
098200     IF MID-TIREFBAT-IN NOT = ALL '+'                                     
098300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIREFBAT-IN-ATTR                 
098400       MOVE MID-TIREFBAT-IN       TO REF-TIREFBAT                         
098500     END-IF                                                               
098600                                                                          
098700     IF MID-REAIRCO-IN  NOT = ALL '+'                                     
098800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REAIRCO-IN-ATTR                  
098900       MOVE W-REAIRCO             TO REF-REAIRCO                          
099000     END-IF                                                               
099100                                                                          
099200     IF MID-KVDLTID-AIRREQ-IN NOT = ALL '+'                               
099300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVDLTID-AIRREQ-IN-ATTR           
099400       MOVE MID-KVDLTID-AIRREQ-IN TO REF-KVDLTID-AIRREQ                   
099500     END-IF                                                               
099600                                                                          
099700     IF MID-BETEXT NOT = ALL '+'                                          
099800       MOVE MID-BETEXT              TO REF-BETEXT                         
099900     END-IF                                                               
099600                                                                          
098700     IF MID-RESSFAC-IN  NOT = ALL '+'                                     
098800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RESSFAC-IN-ATTR                  
098900       MOVE W-RESSFAC             TO REF-RESSFAC                          
099000     END-IF                                                               
099100                                                                          
098700     IF MID-PRFRAKT-IN  NOT = ALL '+'                                     
098800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRFRAKT-IN-ATTR                  
098900       MOVE W-PRFRAKT             TO REF-PRFRAKT                          
099000     END-IF                                                               
100000                                                                          
100100     PERFORM IMS-REPL-WDB616                                              
100200                                                                          
100300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
100400     CALL WMEDKONV USING MED-WMEDAREA                                     
100500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
100600     PERFORM MFS-FORM-ATTR                                                
100700     PERFORM MFS-RENSA-FAELT-IN                                           
100800     .                                                                    
100900     EJECT                                                                
101000 S01-FLYG-PA-HYLLAN SECTION.                                              
101100                                                                          
101200     MOVE ZERO        TO MOD-AIRSHLF                                      
101300     MOVE W-IDDC-REF  TO WS-IDDC                                          
101400     IF NDC OR CDC-SE                                                     
101500        MOVE 2                      TO WORK-KDCALL                        
101600*       MOVE WC-CDC-SE              TO WORK-IDDC                          
101700        MOVE W-IDDC-REF             TO WORK-IDDC                          
101800        MOVE WS-DATUM               TO WORK-TIAAMMDD-FOM                  
101900        MOVE REF-KVDLTID-AIRPAC     TO WORK-KVWORKD                       
102000        ADD +1                      TO WORK-KVWORKD                       
102100                                                                          
102200        CALL WORKDAY USING WORK-KDCALL                                    
102300                  WORK-DATE-AREA WORK-KDSVAR                              
102400                                                                          
102500        IF WORK-KDSVAR-OK                                                 
102600            MOVE WORK-TIAAMMDD-TOM  TO DAG-TIAAMMDD-FOM                   
102700            MOVE 20                 TO DAG-TISEKEL-FOM                    
102800        END-IF                                                            
102900     ELSE                                                                 
103000         MOVE WS-DATUM              TO DAG-TIAAMMDD-FOM                   
103100         MOVE 20                    TO DAG-TISEKEL-FOM                    
103200     END-IF                                                               
103300*                                                                         
103400*    TRANSPORTTID FLYG (KALENDERTID)                                      
103500     MOVE 2                         TO DAG-KDCALL                         
103600     MOVE REF-KVDLTID-AIRTRP        TO DAG-KVKALDAG                       
103700     ADD +1                         TO DAG-KVKALDAG                       
103800*                                                                         
103900                                                                          
104000     CALL WDAGKONV USING DAG-KDCALL                                       
104100                         DAG-DATUM-AREA DAG-KDSVAR                        
104200                                                                          
104300     IF DAG-KDSVAR = SPACE                                                
104400        MOVE DAG-TIAAMMDD-TOM       TO WORK-TIAAMMDD-FOM                  
104500                                                                          
104600*    INLÄGGNINGSTID FLYG (ARBETSTID)                                      
104700        MOVE 2                      TO WORK-KDCALL                        
104800        MOVE W-IDDC                 TO WORK-IDDC                          
104900        MOVE REF-KVDLTID-AIRINS     TO WORK-KVWORKD                       
105000        ADD +1                      TO WORK-KVWORKD                       
105100                                                                          
105200        CALL WORKDAY USING WORK-KDCALL                                    
105300                           WORK-DATE-AREA WORK-KDSVAR                     
105400                                                                          
105500        IF WORK-KDSVAR-OK                                                 
105600           MOVE WORK-TIAAMMDD-TOM   TO MOD-AIRSHLF                        
105700        END-IF                                                            
105800     END-IF                                                               
105900     .                                                                    
106000     EJECT                                                                
106100 S02-BAAT-PA-HYLLAN  SECTION.                                             
106200                                                                          
106300                                                                          
106400*    PACKTID BÅT  (ARBETSTID)                                             
106500     MOVE ZERO        TO MOD-BOATSHLF                                     
106600     MOVE W-IDDC-REF  TO WS-IDDC                                          
106700                                                                          
106800     MOVE 2                         TO WORK-KDCALL                        
106900     MOVE W-IDDC-REF                TO WORK-IDDC                          
107000     MOVE WS-DATUM                  TO WORK-TIAAMMDD-FOM                  
107100     MOVE REF-KVDLTID-BOATPAC       TO WORK-KVWORKD                       
107200     ADD +2                         TO WORK-KVWORKD                       
107300                                                                          
107400     CALL WORKDAY USING WORK-KDCALL                                       
107500                        WORK-DATE-AREA WORK-KDSVAR                        
107600     IF WORK-KDSVAR-OK                                                    
107700        MOVE WORK-TIAAMMDD-TOM      TO DAG-TIAAMMDD-FOM                   
107800        MOVE 20                     TO DAG-TISEKEL-FOM                    
107900*                                                                         
108000*    TRANSPORTTID BÅT  (KALENDERTID)                                      
108100        MOVE 2                      TO DAG-KDCALL                         
108200        MOVE REF-KVDLTID-BOATTRP    TO DAG-KVKALDAG                       
108300        ADD +1                      TO DAG-KVKALDAG                       
108400*                                                                         
108500                                                                          
108600        CALL WDAGKONV USING DAG-KDCALL                                    
108700                            DAG-DATUM-AREA DAG-KDSVAR                     
108800                                                                          
108900        IF DAG-KDSVAR = SPACE                                             
109000           MOVE DAG-TIAAMMDD-TOM    TO WORK-TIAAMMDD-FOM                  
109100                                                                          
109200*    HAMN TILL GRIND     (ARBETSTID)                                      
109300*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
109400           MOVE 2                   TO WORK-KDCALL                        
109500           MOVE W-IDDC              TO WORK-IDDC                          
109600           COMPUTE WORK-KVWORKD =                                         
109700                   REF-KVDLTID-BOAT2DC + REF-KVDLTID-BOATINS              
109800           ADD +1                   TO WORK-KVWORKD                       
109900                                                                          
110000           CALL WORKDAY USING WORK-KDCALL                                 
110100                              WORK-DATE-AREA WORK-KDSVAR                  
110200                                                                          
110300           IF WORK-KDSVAR-OK                                              
110400              MOVE WORK-TIAAMMDD-TOM TO MOD-BOATSHLF                      
110500                                        WS-BOATSHLF                       
110600           END-IF                                                         
110700        END-IF                                                            
110800     END-IF                                                               
110900     .                                                                    
111000     EJECT                                                                
111100 S03-BUFF-PA-HYLLAN  SECTION.                                             
111200                                                                          
111300                                                                          
111400     IF REF-SEG-EXISTS                                                    
111500*    BÅT PÅ HYLLAN FRÅN S02 (ARBETSTID)                                   
111600       IF REF-KVDLTID-BUFF > ZERO                                         
111700                                                                          
111800         MOVE W-IDDC-REF TO WS-IDDC                                       
111900         MOVE 2                       TO WORK-KDCALL                      
112000         MOVE W-IDDC-REF              TO WORK-IDDC                        
112100         MOVE WS-BOATSHLF             TO WORK-TIAAMMDD-FOM                
112200         MOVE REF-KVDLTID-BUFF        TO WORK-KVWORKD                     
112300         ADD +1                       TO WORK-KVWORKD                     
112400                                                                          
112500         CALL WORKDAY USING WORK-KDCALL                                   
112600                            WORK-DATE-AREA WORK-KDSVAR                    
112700         IF WORK-KDSVAR-OK                                                
112800            MOVE WORK-TIAAMMDD-TOM    TO MOD-BUFFSHLF                     
112900         END-IF                                                           
113000       END-IF                                                             
113100     ELSE                                                                 
113200       MOVE ZERO                      TO MOD-BUFFSHLF                     
113300     END-IF                                                               
113400     .                                                                    
113500     EJECT                                                                
113600 MFS-RENSA-FAELT-UT SECTION.                                              
113700                                                                          
113800*    --- ALLA UTDATA-FÄLT                                                 
113900     MOVE MFS-RENSA-FAELT TO MOD-KVDLTID-AIRPAC                           
114000                             MOD-KVDLTID-BOATPAC                          
114100                             MOD-KVDLTID-AIRTRP                           
114200                             MOD-KVDLTID-BOATTRP                          
114300                             MOD-KVDLTID-BOAT2DC                          
114400                             MOD-KVDLTID-AIRINS                           
114500                             MOD-KVDLTID-BOATINS                          
114600                             MOD-KVDLTID-BUFF                             
114700                             MOD-KVDLTID-CUST                             
114800                             MOD-KVDLTID-CUSTWAIT                         
114900                             MOD-KVDLTID-CUST2DC                          
115000                             MOD-KVDLTID-AIRETA                           
115100                             MOD-KVDLTID-TOT                              
115200                             MOD-TIREFBAT                                 
115300                             MOD-REAIRCO                                  
115400                             MOD-KVDLTID-AIRREQ                           
115300                             MOD-RESSFAC                                  
115300                             MOD-PRFRAKT                                  
115500                             MOD-BETEXT                                   
115600     .                                                                    
115700     EJECT                                                                
115800 MFS-RENSA-FAELT-IN SECTION.                                              
115900                                                                          
116000*    --- ALLA INDATA-FÄLT                                                 
116100     MOVE MFS-RENSA-FAELT TO MOD-KVDLTID-AIRPAC-IN                        
116200                             MOD-KVDLTID-BOATPAC-IN                       
116300                             MOD-KVDLTID-AIRTRP-IN                        
116400                             MOD-KVDLTID-BOATTRP-IN                       
116500                             MOD-KVDLTID-BOAT2DC-IN                       
116600                             MOD-KVDLTID-AIRINS-IN                        
116700                             MOD-KVDLTID-BOATINS-IN                       
116800                             MOD-KVDLTID-BUFF-IN                          
116900                             MOD-KVDLTID-CUST-IN                          
117000                             MOD-KVDLTID-CUSTWAIT-IN                      
117100                             MOD-KVDLTID-CUST2DC-IN                       
117200                             MOD-KVDLTID-AIRETA-IN                        
117300                             MOD-KVDLTID-TOT-IN                           
117400                             MOD-TIREFBAT-IN                              
117500                             MOD-REAIRCO-IN                               
117600                             MOD-KVDLTID-AIRREQ-IN                        
115300                             MOD-RESSFAC-IN                               
115300                             MOD-PRFRAKT-IN                               
117700                             MOD-BETEXT                                   
117800     .                                                                    
117900     EJECT                                                                
118000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
118100                                                                          
118200*    --- ALLA UTDATA-FÄLT                                                 
118300     MOVE MFS-ROER-EJ-FAELT TO MOD-KVDLTID-AIRPAC                         
118400                               MOD-KVDLTID-BOATPAC                        
118500                               MOD-KVDLTID-AIRTRP                         
118600                               MOD-KVDLTID-BOATTRP                        
118700                               MOD-KVDLTID-BOAT2DC                        
118800                               MOD-KVDLTID-AIRINS                         
118900                               MOD-KVDLTID-BOATINS                        
119000                               MOD-KVDLTID-BUFF                           
119100                               MOD-KVDLTID-CUST                           
119200                               MOD-KVDLTID-CUSTWAIT                       
119300                               MOD-KVDLTID-CUST2DC                        
119400                               MOD-KVDLTID-AIRETA                         
119500                               MOD-KVDLTID-TOT                            
119600                               MOD-TIREFBAT                               
119700                               MOD-REAIRCO                                
119800                               MOD-KVDLTID-AIRREQ                         
115300                               MOD-RESSFAC                                
115300                               MOD-PRFRAKT                                
119900                               MOD-BETEXT                                 
120000     .                                                                    
120100     EJECT                                                                
120200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
120300                                                                          
120400*    --- ALLA INDATA-FÄLT                                                 
120500     MOVE MFS-ROER-EJ-FAELT TO MOD-KVDLTID-AIRPAC-IN                      
120600                               MOD-KVDLTID-BOATPAC-IN                     
120700                               MOD-KVDLTID-AIRTRP-IN                      
120800                               MOD-KVDLTID-BOATTRP-IN                     
120900                               MOD-KVDLTID-BOAT2DC-IN                     
121000                               MOD-KVDLTID-AIRINS-IN                      
121100                               MOD-KVDLTID-BOATINS-IN                     
121200                               MOD-KVDLTID-BUFF-IN                        
121300                               MOD-KVDLTID-CUST-IN                        
121400                               MOD-KVDLTID-CUSTWAIT-IN                    
121500                               MOD-KVDLTID-CUST2DC-IN                     
121600                               MOD-KVDLTID-AIRETA-IN                      
121700                               MOD-KVDLTID-TOT-IN                         
121800                               MOD-TIREFBAT-IN                            
121900                               MOD-REAIRCO-IN                             
122000                               MOD-KVDLTID-AIRREQ-IN                      
121900                               MOD-RESSFAC-IN                             
121900                               MOD-PRFRAKT-IN                             
122100                               MOD-BETEXT                                 
122200     .                                                                    
122300     EJECT                                                                
122400 MFS-FORM-ATTR SECTION.                                                   
122500                                                                          
122600*    --- ALLA INDATA-FÄLT                                                 
122700     MOVE MFS-FORMATETS-ATTR TO MOD-KVDLTID-AIRPAC-IN-ATTR                
122800                                MOD-KVDLTID-BOATPAC-IN-ATTR               
122900                                MOD-KVDLTID-AIRTRP-IN-ATTR                
123000                                MOD-KVDLTID-BOATTRP-IN-ATTR               
123100                                MOD-KVDLTID-BOAT2DC-IN-ATTR               
123200            MOD-BUFFSHLF        MOD-KVDLTID-AIRINS-IN-ATTR                
123300                                MOD-KVDLTID-BOATINS-IN-ATTR               
123400                                MOD-KVDLTID-BUFF-IN-ATTR                  
123500                                MOD-KVDLTID-CUST-IN-ATTR                  
123600                                MOD-KVDLTID-CUSTWAIT-IN-ATTR              
123700                                MOD-KVDLTID-CUST2DC-IN-ATTR               
123800                                MOD-KVDLTID-AIRETA-IN-ATTR                
123900                                MOD-KVDLTID-TOT-IN-ATTR                   
124000                                MOD-TIREFBAT-IN-ATTR                      
124100                                MOD-REAIRCO-IN-ATTR                       
124200                                MOD-KVDLTID-AIRREQ-IN-ATTR                
124100                                MOD-RESSFAC-IN-ATTR                       
124100                                MOD-PRFRAKT-IN-ATTR                       
124300     .                                                                    
124400     EJECT                                                                
124500* --- IMS SEKTIONER ---                                                   
124600     SKIP3                                                                
124700 IMS-GET-MSG SECTION.                                                     
124800                                                                          
124900     MOVE '  QC' TO GODK-STATUSKODER                                      
125000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
125100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
125200     PERFORM IMS-STATUSKONTROLL                                           
125300     .                                                                    
125400     SKIP3                                                                
125500 IMS-INSERT-MSG SECTION.                                                  
125600                                                                          
125700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
125800     MOVE SPACE TO GODK-STATUSKODER                                       
125900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
126000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126100     PERFORM IMS-STATUSKONTROLL                                           
126200     .                                                                    
126300     EJECT                                                                
126400 IMS-GU-WDB601 SECTION.                                                   
126500                                                                          
126600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
126700          DELIMITED BY SIZE INTO SSA1                                     
126800     MOVE '  GE' TO GODK-STATUSKODER                                      
126900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
127000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
127100     PERFORM IMS-STATUSKONTROLL                                           
127200     .                                                                    
127300     SKIP3                                                                
127400 IMS-GHU-WDB601 SECTION.                                                  
127500                                                                          
127600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
127700          DELIMITED BY SIZE INTO SSA1                                     
127800     MOVE '  ' TO GODK-STATUSKODER                                        
127900     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
128000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
128100     PERFORM IMS-STATUSKONTROLL                                           
128200     .                                                                    
128300     SKIP3                                                                
128400 IMS-GU-WDB615 SECTION.                                                   
128500                                                                          
128600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
128700          DELIMITED BY SIZE INTO SSA1                                     
128800     STRING 'WDB615  (WDB615KY =' W-WDB615KY-X ')'                        
128900          DELIMITED BY SIZE INTO SSA2                                     
129000     MOVE '  GE' TO GODK-STATUSKODER                                      
129100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2               
129200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
129300     PERFORM IMS-STATUSKONTROLL                                           
129400     .                                                                    
129500     SKIP3                                                                
129600 IMS-GHU-WDB615 SECTION.                                                  
129700                                                                          
129800     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
129900          DELIMITED BY SIZE INTO SSA1                                     
130000     STRING 'WDB615  (WDB615KY =' W-WDB615KY-X ')'                        
130100          DELIMITED BY SIZE INTO SSA2                                     
130200     MOVE '  GE' TO GODK-STATUSKODER                                      
130300     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2              
130400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
130500     PERFORM IMS-STATUSKONTROLL                                           
130600     .                                                                    
130700     SKIP3                                                                
130800 IMS-REPL-WDB615 SECTION.                                                 
130900                                                                          
131000     MOVE '  ' TO GODK-STATUSKODER                                        
131100     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB615                       
131200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
131300     PERFORM IMS-STATUSKONTROLL                                           
131400     .                                                                    
131500     EJECT                                                                
131600 IMS-ISRT-WDB615 SECTION.                                                 
131700                                                                          
131800     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
131900          DELIMITED BY SIZE INTO SSA1                                     
132000     MOVE 'WDB615   '         TO SSA2                                     
132100     MOVE '  ' TO GODK-STATUSKODER                                        
132200     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB615 SSA1 SSA2             
132300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
132400     PERFORM IMS-STATUSKONTROLL                                           
132500     .                                                                    
132600     EJECT                                                                
132700 IMS-GU-WDB616 SECTION.                                                   
132800                                                                          
132900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
133000          DELIMITED BY SIZE INTO SSA1                                     
133100     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
133200          DELIMITED BY SIZE INTO SSA2                                     
133300     MOVE '  GE' TO GODK-STATUSKODER                                      
133400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
133500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
133600     PERFORM IMS-STATUSKONTROLL                                           
133700     .                                                                    
133800     SKIP3                                                                
133900 IMS-GHU-WDB616 SECTION.                                                  
134000                                                                          
134100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
134200          DELIMITED BY SIZE INTO SSA1                                     
134300     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
134400          DELIMITED BY SIZE INTO SSA2                                     
134500     MOVE '  GE' TO GODK-STATUSKODER                                      
134600     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2              
134700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000     SKIP3                                                                
135100 IMS-ISRT-WDB616 SECTION.                                                 
135200                                                                          
135300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
135400          DELIMITED BY SIZE INTO SSA1                                     
135500     MOVE 'WDB616   '         TO SSA2                                     
135600     MOVE '  ' TO GODK-STATUSKODER                                        
135700     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB616 SSA1 SSA2             
135800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
135900     PERFORM IMS-STATUSKONTROLL                                           
136000     .                                                                    
136100     EJECT                                                                
136200 IMS-REPL-WDB616 SECTION.                                                 
136300                                                                          
136400     MOVE '  ' TO GODK-STATUSKODER                                        
136500     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB616                       
136600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
136700     PERFORM IMS-STATUSKONTROLL                                           
136800     .                                                                    
136900     EJECT                                                                
137000 IMS-STATUSKONTROLL SECTION.                                              
137100                                                                          
137200     SET STATUS-IX TO 1                                                   
137300     SEARCH GODK-STATUS                                                   
137400       AT END                                                             
137500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
137600         DELIMITED BY SIZE INTO FELTEXT                                   
137700         CALL FELLOG                                                      
137800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
137900         CONTINUE                                                         
138000     END-SEARCH                                                           
138100     .                                                                    
