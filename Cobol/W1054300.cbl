000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1054300.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   2000/02/11.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        Visar information om bilmodeller och årsintervall                
000900*        för en artikel i en katalog.                                     
001000*        Nycklar är IDARTNR och IDCATNR                                   
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDN1                                       
001300*        PROGRAMMET LÄSER      WDN6                                       
001500*        PROGRAMMET LÄSER      WDD3                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W1T543                                              
001900*        MID:         W1I54301                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W1O54301                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W1054300'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  SPRAK-IDSKYLT               PIC X(3)    VALUE SPACE.                 
004000     88  SVENSKA    VALUE 'S  '.                                          
004100     88  ENGELSKA   VALUE 'GB '.                                          
004200                                                                          
004300*    --- INDEX FÖR BLÄDDRINGSRADER OCH KOLUMNER                           
004400 77  MOD-RAD                 PIC S9(4)  VALUE +0    COMP-3.               
004500 77  MAX-MOD-RAD             PIC S9(4)  VALUE +11   COMP-3.               
004600 77  MOD-KOL                 PIC S9(4)  VALUE +0    COMP-3.               
004700 77  MAX-MOD-KOL             PIC S9(4)  VALUE +2    COMP-3.               
004800                                                                          
004810*    --- INDEX FÖR RADER OCH KOLUMNER PÅ 1542-MIDDEN                      
004900 01  RAD                     PIC S9(4)  VALUE +0    COMP-3.               
005000 01  MAX-RAD                 PIC S9(4)  VALUE +13   COMP-3.               
005100 01  KOL                     PIC S9(4)  VALUE +0    COMP-3.               
005200 01  MAX-KOL                 PIC S9(4)  VALUE +3    COMP-3.               
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '1543'.                
006200     88  GODK-MID                            VALUE '1541'                 
006300                                                   '1542'                 
006400                                                   '1543'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007310                                                                          
007400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007901     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '106'.                 
007910     03  INF-INFO-MISSING        PIC X(3)    VALUE '413'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     EJECT                                                                
008110                                                                          
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008710                                                                          
008800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008900*    -------  GAMLA SKÄRMRAD 4  --------------                            
009000 01  SPAR-AREA.                                                           
009100     03  SPAR-IDTRANS                 PIC X(4)  VALUE '1543'.             
009200     03  SPAR-WDN611KY-ENTER.                                             
009300       05  SPAR-IDFORDON-ENTER        PIC S9(3) COMP-3.                   
009400       05  SPAR-TIOMBRYT-9KOMPL-ENTER PIC S9(7) COMP-3.                   
009500                                                                          
009600     03  SPAR-WDN611KY-NEXT.                                              
009700       05  SPAR-IDFORDON-NEXT         PIC S9(3) COMP-3.                   
009800       05  SPAR-TIOMBRYT-9KOMPL-NEXT  PIC S9(7) COMP-3.                   
009900                                                                          
010000     03  SPAR-IDMODELL-ENTER          PIC X(3).                           
010100     03  SPAR-IDMODELL-NEXT           PIC X(3).                           
010200     EJECT                                                                
010300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010400 01  FILLER                    PIC X(16) VALUE '1543-MID-AREA'.           
010510 01  1543-MID-AREA             PIC X(300).                                
010600*01  MID -COPY W1I54301  -RED 1543-MID-AREA                               
010700     EJECT                                                                
011100 01 FILLER                     PIC X(16) VALUE '1542-MID-AREA'.           
011200*01  MID -COPY W1I54201  -PRE 1542-.                                      
011300     EJECT                                                                
011310 01  FILLER                    PIC X(16) VALUE '1541-MID-AREA'.           
011320*01  MID -COPY W1I54101  -PRE 1541-.                                      
011330     EJECT                                                                
011400 01  FILLER                    PIC X(16)  VALUE 'MSG/MOD-AREA'.           
011500     SKIP3                                                                
011600*01  -COPY WMSGAREA                                                       
011700     EJECT                                                                
011800     03  MOD REDEFINES MSG-AREA.                                          
011900*      05  -COPY W1O54301                                                 
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012200     SKIP3                                                                
012300*01  -COPY WMFSAREA                                                       
012400     EJECT                                                                
012500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013000 01  NYCKLAR-TILL-DLI.                                                    
013100                                                                          
013700     03  W-IDCATNR-X.                                                     
013800         05  W-IDCATNR           PIC 9(5)    VALUE ZERO.                  
013900*                                                                         
014300     03  W-IDARTNR-X.                                                     
014400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014500                                                                          
015300     03  FILLER           PIC X(16)  VALUE 'BLÄDDR.NYCKLAR  '.            
015310*    --- Bläddringsnycklar för första raden på skärmen                    
015330     03  W-WDN611KY-MIN-X.                                                
015340         05  W-IDFORDON-MIN        PIC S9(3) COMP-3 VALUE ZERO.           
015350         05  W-TIOMBRYT-9KOMPL-MIN PIC S9(7) COMP-3 VALUE ZERO.           
015370     03  W-IDMODELL-MIN-X.                                                
015380         05  W-IDMODELL-MIN      PIC X(3)    VALUE SPACE.                 
015520*    -- Jämförfält för WDN611-segmentet (innehåller mid-idkatnr)          
015530     03  WS-IDKATNR-X.                                                    
015540         05  WS-IDKATNR          PIC S9(5)  VALUE ZERO COMP-3.            
015550*                                                                         
015610     03  W-IDSKYLT-X.                                                     
015620         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
015621     03  W-BEART-X.                                                       
015622         05  W-BEART             PIC X(25)   VALUE SPACE.                 
015623                                                                          
016400     SKIP2                                                                
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016910     88  BASEN-SLUT                          VALUE 'GB'.                  
017000     SKIP2                                                                
017100 01  GODK-STATUSKODER.                                                    
017200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017300     SKIP3                                                                
017400 01  SSA1                        PIC X(64).                               
017500 01  SSA2                        PIC X(64).                               
017510 01  SSA3                        PIC X(64).                               
017600     EJECT                                                                
017700*    --- IMS FUNKTIONSKODER                                               
017800*01  -COPY W0003                                                          
017900     EJECT                                                                
018000*    ---  DLI INPUT-OUTPUT AREA                                           
018100                                                                          
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN101'.                      
018300 01  DLI-IO-WDN101.                                                       
018400*    03  -COPY WDN101  -PRE WDN101- .                                     
018500     EJECT                                                                
018600                                                                          
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
019200 01  DLI-IO-WDN601.                                                       
019300*    03  -COPY WDN601 -PRE WDN601-                                        
019400     EJECT                                                                
019500                                                                          
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
019610 01  FILLER         PIC X(16) VALUE 'OCH  IO-WDN621'.                     
019620 01  DLI-IO-WDN611-21.                                                    
019700     03 DLI-IO-WDN611.                                                    
019800*        05 -COPY WDN611 -PRE WDN611-                                     
020000                                                                          
020700     03 DLI-IO-WDN621.                                                    
020800*        05 -COPY WDN621 -PRE WDN621-                                     
021300                                                                          
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN612'.                      
021500 01  DLI-IO-WDN612.                                                       
021600*    03  -COPY WDN612  -PRE WDN612-                                       
021700     EJECT                                                                
021710                                                                          
021800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
021810 01  DLI-IO-WDD301.                                                       
021900*    03  -COPY WDD301   -PRE WDD301- .                                    
022000     EJECT                                                                
022010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
022020 01  DLI-IO-WDD311.                                                       
022030*    03  -COPY WDD311   -PRE WDD311- .                                    
022040     EJECT                                                                
022100 LINKAGE SECTION.                                                         
022200*01  -COPY W0009   -PRE MSG-                                              
022300*01  -COPY W0008   -PRE USEA-                                             
022400     05  FILLER                  PIC X.                                   
022500                                                                          
022600*01  -COPY W0008  -PRE WDN1-                                              
022700     05  FILLER                  PIC X.                                   
022800                                                                          
022900*01  -COPY W0008  -PRE WDN6-                                              
023000     05  FILLER                  PIC X.                                   
023100                                                                          
023500*01  -COPY W0008  -PRE WDD3A-                                             
023600     05  FILLER                  PIC X.                                   
023700                                                                          
023800*01  -COPY W0008  -PRE WDD3B-                                             
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDN1-PCB WDN6-PCB             
024200     WDD3A-PCB WDD3B-PCB.                                                 
024300 MAIN SECTION.                                                            
024400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDN1-PCB WDN6-PCB             
024500     WDD3A-PCB WDD3B-PCB.                                                 
024600                                                                          
024700     PERFORM IMS-GET-MSG                                                  
024800     IF SEGMENT-FINNS                                                     
024900       PERFORM A-INIT                                                     
025000       PERFORM B-KOLLA-NYCKLAR                                            
025100       IF NYCKLAR-OK                                                      
025200         IF MFS-FIRST                                                     
025300           PERFORM C-FOERSTA-SIDA                                         
025400         ELSE                                                             
025500           IF MFS-NEXT                                                    
025600             PERFORM D-NAESTA-SIDA                                        
025700           ELSE                                                           
025800             PERFORM E-SAMMA-SIDA                                         
025900           END-IF                                                         
026000         END-IF                                                           
026100                                                                          
026200         PERFORM F-LAES-VISA-INFO                                         
026300       END-IF                                                             
026400*      --- IF PROGRAM-TO-PROGRAM-SWITCH: = MOD-LENGTH + 17                
026500       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O54301 + 4                      
026600       PERFORM IMS-INSERT-MSG                                             
026700     END-IF                                                               
026800                                                                          
026900     MOVE ZERO TO RETURN-CODE                                             
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 A-INIT SECTION.                                                          
027400                                                                          
027500     IF MSG-DUBBLA-TRANSKODER                                             
027600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO 1543-MID-AREA                
027700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027900     ELSE                                                                 
028000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO 1543-MID-AREA                 
028100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
028200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028300     END-IF                                                               
028400                                                                          
028500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028800                                                                          
028900     MOVE LOW-VALUE TO MSG-AREA                                           
029000     MOVE 'W1O543N1' TO MFS-IDMOD                                         
029100     MOVE '1543' TO MOD-IDTRANS                                           
029200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029300                                                                          
029400     IF EGEN-MID OR HELP-MID                                              
029500       CONTINUE                                                           
029600     ELSE                                                                 
029700       IF W-IDTRANS = '1541'                                              
029800*          -- Fixar nyckel IDCATNR för hopp från 1541                     
029810*          -- Hämtar IDCATNR från utdatarad 5                             
029900           MOVE 1543-MID-AREA TO  1541-MID-W1I54101                       
030000           INSPECT 1541-MID-IDCATNR REPLACING LEADING                     
030100                                    SPACE BY ZERO                         
030200           MOVE 1541-MID-IDCATNR  TO MID-IDCATNR-IN                       
030300       END-IF                                                             
030400                                                                          
030500       IF W-IDTRANS = '1542'                                              
030700         MOVE 1543-MID-AREA TO 1542-MID-W1I54201                          
030701*        -- Flytta nycklar som inte matchar mellan middarna               
030710         MOVE 1542-MID-IDKATNR-IN TO MID-IDCATNR-IN                       
030720         MOVE 1542-MID-IDKATNR-UT TO MID-IDCATNR-UT                       
030730                                                                          
030800         MOVE +1 TO KOL, RAD                                              
030900*******  -- Sök kolumnvis om KDCMD är markerad                            
030910*******  -- då skall man läsa med den katalog som står där                
030920*******  -- annars tar man IDCATNR-IN/UT som vanligt.                     
030960                                                                          
031000         PERFORM UNTIL KOL > MAX-KOL                                      
031100             PERFORM UNTIL RAD > MAX-RAD                                  
031200                IF 1542-MID-KDCMD (RAD KOL) NOT = SPACE AND '+'           
031300                  INSPECT 1542-MID-IDCATNR (RAD KOL)                      
031400                                REPLACING LEADING SPACE BY ZERO           
031500                  MOVE 1542-MID-IDCATNR (RAD KOL) TO                      
031600                                                 MID-IDCATNR-IN           
031700                  MOVE MAX-RAD TO RAD                                     
031800                  MOVE MAX-KOL TO KOL                                     
031900                END-IF                                                    
032000                ADD +1 TO RAD                                             
032100             END-PERFORM                                                  
032210             MOVE +1 TO RAD                                               
032220             ADD  +1 TO KOL                                               
032300         END-PERFORM                                                      
032500       END-IF                                                             
032600                                                                          
032700       MOVE SPACE TO MFS-KDTRTYP                                          
032800       MOVE '7' TO MFS-IDPFK                                              
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 B-KOLLA-NYCKLAR SECTION.                                                 
033300                                                                          
033400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033500     MOVE '001'             TO MSGI-KDCALL                                
033600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033800     MOVE '1543'            TO MSGI-IDTRANS                               
033900     IF GODK-MID                                                          
034000         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
034100         MOVE MID-IDCATNR-IN     TO MSGI-IDCATNR                          
034200     END-IF                                                               
034300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034310                                                                          
034320     MOVE MSGI-SPAR-AREA  TO SPAR-AREA                                    
034330                                                                          
034400     MOVE JA TO NYCKLAR-SW                                                
034500                                                                          
034600     IF MSGI-IDLAND-SPR = 'SE'                                            
034700        SET SVENSKA  TO TRUE                                              
034800     ELSE                                                                 
034900        SET ENGELSKA TO TRUE                                              
035000     END-IF                                                               
035010     MOVE SPRAK-IDSKYLT TO MED-IDSKYLT                                    
035100                                                                          
035200*    -- KONTROLL AV IDCATNR                                               
035300     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
035400                                                                          
035500     IF MID-IDCATNR-IN NOT = ALL '+'                                      
035600       MOVE '7'         TO MFS-IDPFK                                      
035700       MOVE SPACE       TO MFS-KDTRTYP                                    
035800     END-IF                                                               
035810     INSPECT MSGI-IDCATNR REPLACING ALL SPACE BY ZERO                     
035811                                    ALL '+'   BY ZERO                     
035820     IF MSGI-IDCATNR NUMERIC                                              
035830       MOVE MSGI-IDCATNR TO W-IDCATNR                                     
035831                           WS-IDKATNR                                     
035840     ELSE                                                                 
035850       MOVE NEJ TO NYCKLAR-SW                                             
035860     END-IF                                                               
036000                                                                          
036100*    -- KONTROLL AV IDARTNR                                               
036200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
036300                                                                          
036400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
036500       MOVE '7'         TO MFS-IDPFK                                      
036600       MOVE SPACE       TO MFS-KDTRTYP                                    
036700     END-IF                                                               
036800     INSPECT MSGI-IDARTNR REPLACING ALL SPACE BY ZERO                     
036900     IF MSGI-IDARTNR NUMERIC                                              
037000       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
037100     ELSE                                                                 
037200       MOVE NEJ TO NYCKLAR-SW                                             
037300     END-IF                                                               
037400                                                                          
037500     IF GODK-MID OR NYCKLAR-OK                                            
037600       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
037700       MOVE MSGI-IDCATNR    TO MOD-IDCATNR-UT                             
037720       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
037730       INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE             
037800     ELSE                                                                 
037900       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
038000       MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-UT                             
038100     END-IF                                                               
038200                                                                          
038300     IF NYCKLAR-FEL                                                       
038400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
038500       CALL WMEDKONV USING MED-WMEDAREA                                   
038600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
038700       PERFORM MFS-RENSA-FAELT-IN                                         
038800       PERFORM MFS-RENSA-FAELT-UT                                         
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 C-FOERSTA-SIDA SECTION.                                                  
039300                                                                          
039400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
039500     CALL WMEDKONV USING MED-WMEDAREA                                     
039600     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
039700                                                                          
039800     PERFORM MFS-RENSA-FAELT-IN                                           
039900     .                                                                    
040000     EJECT                                                                
040100 D-NAESTA-SIDA SECTION.                                                   
040200                                                                          
040300     IF SPAR-IDTRANS = '1543'                                             
040400*      --- WDN611-nyckel                                                  
040500       MOVE SPAR-WDN611KY-NEXT TO W-WDN611KY-MIN-X                        
040600*      --- WDN621-nyckel                                                  
040700       MOVE SPAR-IDMODELL-NEXT TO W-IDMODELL-MIN                          
040800     ELSE                                                                 
040900       PERFORM MFS-RENSA-FAELT-IN                                         
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 E-SAMMA-SIDA SECTION.                                                    
041400                                                                          
041500     IF SPAR-IDTRANS = '1543' OR '0551'                                   
041600*      --- WDN611-nyckel                                                  
041700       MOVE SPAR-WDN611KY-ENTER TO W-WDN611KY-MIN-X                       
041800*      --- WDN621-nyckel                                                  
041900       MOVE SPAR-IDMODELL-ENTER TO W-IDMODELL-MIN                         
042000     ELSE                                                                 
042100       PERFORM MFS-RENSA-FAELT-IN                                         
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 F-LAES-VISA-INFO SECTION.                                                
042600                                                                          
042700     PERFORM FA-LAES-GRUNDDATA                                            
042800                                                                          
042900     IF SEGMENT-SAKNAS                                                    
043000*       -- Artikeln saknas på MASTER WDN601                               
043010        MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                               
043100        CALL WMEDKONV USING MED-WMEDAREA                                  
043200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
043300        PERFORM MFS-RENSA-FAELT-UT                                        
043400     ELSE                                                                 
043500*      -- Läs första WDN611 för data till första raden                    
044200       PERFORM IMS-GU-WDN601                                              
044201       IF MFS-FIRST                                                       
044203*        -- Det finns ALLTID minst ett WDN611-segment                     
044204         PERFORM IMS-GNP-WDN611-OKVAL                                     
044205         IF W-IDCATNR > ZERO                                              
044206*          -- Hitta rätt 11-segment för inmatad katalog                   
044207           PERFORM UNTIL WDN611-KAT-IDKATNR = WS-IDKATNR                  
044208                      OR SEGMENT-SAKNAS OR BASEN-SLUT                     
044209             PERFORM IMS-GNP-WDN611-OKVAL                                 
044210           END-PERFORM                                                    
044211           IF SEGMENT-FINNS                                               
044212*            -- Spara nyckelvärden för KVAL-läsning                       
044213             MOVE WDN611-KAT-IDFORDON    TO W-IDFORDON-MIN                
044214             MOVE WDN611-KAT-TIOMBRYT-9KOMPL                              
044215                                         TO W-TIOMBRYT-9KOMPL-MIN         
044216           ELSE                                                           
044218             MOVE ZERO                   TO W-IDFORDON-MIN                
044219                                            W-TIOMBRYT-9KOMPL-MIN         
044220*            -- Fel IDCATNR-IN                                            
044221             MOVE ZERO            TO W-IDCATNR  WS-IDKATNR                
044222             MOVE MFS-RENSA-FAELT TO MOD-BEMASTER                         
044223                                     MOD-IDCATNR-UT                       
044224           END-IF                                                         
044225         ELSE                                                             
044226           MOVE WDN611-KAT-IDFORDON      TO W-IDFORDON-MIN                
044227           MOVE WDN611-KAT-TIOMBRYT-9KOMPL                                
044228                                         TO W-TIOMBRYT-9KOMPL-MIN         
044229         END-IF                                                           
044230       END-IF                                                             
044231*      -- Första dataläsningen                                            
044232       PERFORM IMS-GU-WDN601                                              
044233       IF W-IDFORDON-MIN = ZERO                                           
044234         PERFORM IMS-GNP-WDN611-OKVAL                                     
044235       ELSE                                                               
044236         PERFORM IMS-GNP-WDN611-KVAL                                      
044237       END-IF                                                             
044238       PERFORM FB-LAES-WDN621-MODELL-DATA                                 
044240                                                                          
044300       IF SEGMENT-FINNS                                                   
044400         MOVE WDN611-KAT-IDFORDON TO SPAR-IDFORDON-ENTER                  
044500         MOVE WDN611-KAT-TIOMBRYT-9KOMPL                                  
044600                                  TO SPAR-TIOMBRYT-9KOMPL-ENTER           
044700         MOVE WDN621-MDL-IDMODELL TO SPAR-IDMODELL-ENTER                  
044800       ELSE                                                               
044900         MOVE W-WDN611KY-MIN-X    TO SPAR-WDN611KY-ENTER                  
045000         MOVE W-IDMODELL-MIN      TO SPAR-IDMODELL-ENTER                  
045100       END-IF                                                             
045200                                                                          
045210       MOVE +1 TO MOD-KOL                                                 
045220                  MOD-RAD                                                 
045300       PERFORM UNTIL MOD-KOL > MAX-MOD-KOL                                
045310         PERFORM UNTIL MOD-RAD > MAX-MOD-RAD                              
045400           IF SEGMENT-FINNS                                               
045500             MOVE WDN621-MDL-IDMODELL                                     
045510                            TO MOD-IDMODELL     (MOD-KOL MOD-RAD)         
045600             MOVE WDN621-MDL-BEMODELL                                     
045610                            TO MOD-BEMODELL     (MOD-KOL MOD-RAD)         
045700             MOVE WDN621-MDL-TIMODAAR-STA                                 
045800                             TO MOD-TIMODAAR-STA(MOD-KOL MOD-RAD)         
045900             MOVE WDN621-MDL-TIMODAAR-STO                                 
046000                             TO MOD-TIMODAAR-STO(MOD-KOL MOD-RAD)         
046100             MOVE WDN611-KAT-IDKATNR                                      
046110                             TO MOD-IDCATNR     (MOD-KOL MOD-RAD)         
046200             PERFORM FB-LAES-WDN621-MODELL-DATA                           
046300           ELSE                                                           
046400             MOVE MFS-RENSA-FAELT TO MOD-IDCATNR (MOD-KOL MOD-RAD)        
046500                                  MOD-IDMODELL (MOD-KOL MOD-RAD)          
046600                                MOD-TIMODAAR-STA (MOD-KOL MOD-RAD)        
046700                                MOD-TIMODAAR-STO (MOD-KOL MOD-RAD)        
046800                                  MOD-IDCATNR (MOD-KOL MOD-RAD)           
046900           END-IF                                                         
047000           ADD +1 TO MOD-RAD                                              
047001         END-PERFORM                                                      
047020         MOVE +1 TO MOD-RAD                                               
047030         ADD  +1 TO MOD-KOL                                               
047100       END-PERFORM                                                        
047200                                                                          
047300       IF SEGMENT-FINNS                                                   
047400         MOVE WDN611-KAT-IDFORDON TO SPAR-IDFORDON-NEXT                   
047500         MOVE WDN611-KAT-TIOMBRYT-9KOMPL                                  
047600                                  TO SPAR-TIOMBRYT-9KOMPL-NEXT            
047700         MOVE WDN621-MDL-IDMODELL TO SPAR-IDMODELL-NEXT                   
047800                                                                          
047900         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
048000         CALL WMEDKONV USING MED-WMEDAREA                                 
048100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
048200       ELSE                                                               
048300         MOVE ZERO  TO SPAR-IDFORDON-NEXT                                 
048320                       SPAR-TIOMBRYT-9KOMPL-NEXT                          
048400         MOVE SPACE TO SPAR-IDMODELL-NEXT                                 
048401                                                                          
048410         MOVE INF-LAST-PAGE-SHOWN  TO MED-IDMFSINF                        
048420         CALL WMEDKONV USING MED-WMEDAREA                                 
048430         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
048500       END-IF                                                             
048600                                                                          
048700       MOVE '002'      TO MSGI-KDCALL                                     
048800       MOVE '1543'     TO SPAR-IDTRANS                                    
048900       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
049000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 FA-LAES-GRUNDDATA SECTION.                                               
049500                                                                          
049600*    -- Läs Artikeln på WDN601.                                           
049601*           Spara ev. benämning från WDN612.                              
049610*    -- Läs Katalogen på WDN1 för att lägga ut BEMASTER                   
049800*    -- Läs Artikeln på WDD3B för att lägga ut BEART på rätt SPR.         
049900*           Om den saknas där,  läs WDD3A med S-benämningen från          
050000*           WDN612 och hämta GB om det behövs.                            
050100                                                                          
050200     PERFORM IMS-GU-WDN601                                                
050300     IF SEGMENT-FINNS                                                     
050400       IF W-IDCATNR > ZERO                                                
052000*        -- Hämta kataloginfo på WDN101                                   
052100         PERFORM IMS-GET-WDN101                                           
052200         IF SEGMENT-FINNS                                                 
052210           MOVE WDN101-KAT-BEMASTER TO MOD-BEMASTER                       
052300         ELSE                                                             
052301*          -- Sökt katalog var inte registrerad                           
052310           MOVE MFS-RENSA-FAELT TO MOD-BEMASTER                           
052330           MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                            
052340           CALL WMEDKONV USING MED-WMEDAREA                               
052350           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
052400         END-IF                                                           
052900       END-IF                                                             
053100*      -- Spara eventuellt benämnings-segment från MASTER,,               
053300       PERFORM IMS-GNP-WDN612                                             
053310       IF SEGMENT-SAKNAS                                                  
053320           MOVE SPACE TO  WDN612-BEN-BEART                                
053330       END-IF                                                             
053400*      -- Men benämningen på BENREG går före                              
053500       MOVE SPRAK-IDSKYLT TO W-IDSKYLT                                    
053600       PERFORM IMS-GU-WDD3BSEQ-TEXT                                       
053700       IF SEGMENT-FINNS                                                   
053800         MOVE WDD311-TEXT-BEART TO MOD-BEART                              
053900       ELSE                                                               
054000         IF WDN612-BEN-BEART NOT = SPACE                                  
054010           IF ENGELSKA                                                    
054020*            -- Översätt den Svenska benämningen via WDD3                 
054100             MOVE WDN612-BEN-BEART TO W-BEART                             
054200             MOVE 'S  '            TO W-IDSKYLT                           
054300             PERFORM IMS-GU-WDD301-ASEQ                                   
054400             IF SEGMENT-FINNS                                             
054500               MOVE 'GB ' TO W-IDSKYLT                                    
054600               PERFORM IMS-GNP-WDD311                                     
054700               IF SEGMENT-FINNS                                           
054800                 MOVE WDD311-TEXT-BEART TO MOD-BEART                      
055100               END-IF                                                     
055200             ELSE                                                         
055300               MOVE SPACE TO MOD-BEART                                    
055400             END-IF                                                       
055401           ELSE                                                           
055402             MOVE WDN612-BEN-BEART TO MOD-BEART                           
055410           END-IF                                                         
055500         END-IF                                                           
055600       END-IF                                                             
055601                                                                          
055610       SET SEGMENT-FINNS TO TRUE                                          
055620*      --- Eftersom  WDN601 fanns                                         
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 FB-LAES-WDN621-MODELL-DATA  SECTION.                                     
056100                                                                          
056110     PERFORM IMS-GNP-WDN621                                               
056120     IF SEGMENT-SAKNAS                                                    
056140                                                                          
056200       IF W-IDCATNR > ZERO                                                
056210*        -- Får inte läsa vidare under nästa WDN611                       
056220         CONTINUE                                                         
056430       ELSE                                                               
056431         PERFORM IMS-GNP-WDN621-PATH                                      
056447       END-IF                                                             
056450     END-IF                                                               
056600     .                                                                    
056700     EJECT                                                                
056800 MFS-RENSA-FAELT-UT SECTION.                                              
056900*    --- ALLA UTDATA-FÄLT                                                 
057000*    --- INKL. BLÄDDRINGSNYCKLAR                                          
057100     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
057110     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
057200                             MOD-BEMASTER                                 
057220     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
057300     .                                                                    
057400     EJECT                                                                
057500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
057600                                                                          
057700     MOVE +1 TO MOD-RAD MOD-KOL                                           
057800     PERFORM UNTIL MOD-KOL > MAX-MOD-KOL                                  
057900       PERFORM UNTIL MOD-RAD > MAX-MOD-RAD                                
058000*        --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                               
058010         MOVE MFS-RENSA-FAELT TO MOD-IDMODELL    (MOD-KOL MOD-RAD)        
058020                                 MOD-BEMODELL    (MOD-KOL MOD-RAD)        
058030                                 MOD-TIMODAAR-STA(MOD-KOL MOD-RAD)        
058040                                 MOD-TIMODAAR-STO(MOD-KOL MOD-RAD)        
058050                                 MOD-IDCATNR     (MOD-KOL MOD-RAD)        
058600         ADD +1 TO MOD-RAD                                                
058700       END-PERFORM                                                        
058800       ADD +1 TO MOD-KOL                                                  
058900       MOVE +1 TO MOD-RAD                                                 
059000     END-PERFORM                                                          
059100     .                                                                    
059200     EJECT                                                                
059300 MFS-RENSA-FAELT-IN SECTION.                                              
059400                                                                          
059500*    --- ALLA INDATA-FÄLT                                                 
059600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
059700                             MOD-IDCATNR-IN                               
059800     .                                                                    
059900     EJECT                                                                
064600* --- IMS SEKTIONER ---                                                   
064700     SKIP3                                                                
064800 IMS-GET-MSG SECTION.                                                     
064900                                                                          
065000     MOVE '  QC' TO GODK-STATUSKODER                                      
065100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
065200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065300     PERFORM IMS-STATUSKONTROLL                                           
065400     .                                                                    
065500     SKIP3                                                                
065600 IMS-INSERT-MSG SECTION.                                                  
065700                                                                          
065800     IF MSGI-IDLAND-SPR = 'SE'                                            
065900       MOVE '0' TO MFS-KDHUVOMR                                           
066000     END-IF                                                               
066100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
066200     MOVE SPACE TO GODK-STATUSKODER                                       
066300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
066400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066500     PERFORM IMS-STATUSKONTROLL                                           
066600     .                                                                    
066700     EJECT                                                                
066800 IMS-GET-WDN101 SECTION.                                                  
066900                                                                          
067000     STRING 'WDN101  (IDCATNR =>' W-IDCATNR-X ')'                         
067100          DELIMITED BY SIZE INTO SSA1                                     
067200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
067300     CALL CBLTDLI USING GU WDN1-PCB DLI-IO-WDN101 SSA1                    
067400     MOVE WDN1-STATUS-CODE TO STATUS-WS                                   
067500     PERFORM IMS-STATUSKONTROLL                                           
067600     .                                                                    
067700     EJECT                                                                
068800 IMS-GU-WDN601 SECTION.                                                   
068900                                                                          
069000     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
069100          DELIMITED BY SIZE INTO SSA1                                     
069200     MOVE '  GE' TO GODK-STATUSKODER                                      
069300     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
069400     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
069500     PERFORM IMS-STATUSKONTROLL                                           
069600     .                                                                    
069700     EJECT                                                                
069800 IMS-GNP-WDN611-OKVAL SECTION.                                            
069900                                                                          
070000     MOVE 'WDN611  '  TO SSA1                                             
070200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
070300     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
070400     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
070500     PERFORM IMS-STATUSKONTROLL                                           
070600     .                                                                    
070700     SKIP2                                                                
070800 IMS-GNP-WDN611-KVAL SECTION.                                             
070900                                                                          
071000     STRING 'WDN611  (WDN611KY =' W-WDN611KY-MIN-X ')'                    
071100          DELIMITED BY SIZE INTO SSA1                                     
071200     MOVE '  GE' TO GODK-STATUSKODER                                      
071300     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
071400     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071610     EJECT                                                                
071620 IMS-GNP-WDN621-PATH  SECTION.                                            
071630                                                                          
071640     MOVE 'WDN611  *D'  TO SSA1                                           
071641     MOVE 'WDN621  '  TO SSA2                                             
071650     MOVE '  GEGB' TO GODK-STATUSKODER                                    
071660     CALL CBLTDLI USING GNP WDN6-PCB  DLI-IO-WDN611-21                    
071661                            SSA1  SSA2                                    
071670     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
071680     PERFORM IMS-STATUSKONTROLL                                           
071690     .                                                                    
071691     SKIP2                                                                
071700 IMS-GNP-WDN621 SECTION.                                                  
071800                                                                          
071810     STRING 'WDN611  (WDN611KY =' W-WDN611KY-MIN-X ')'                    
071820          DELIMITED BY SIZE INTO SSA1                                     
071900     STRING 'WDN621  (IDMODELL>=' W-IDMODELL-MIN-X ')'                    
072000          DELIMITED BY SIZE INTO SSA2                                     
072100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
072200     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN621 SSA1 SSA2              
072300     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
072400     PERFORM IMS-STATUSKONTROLL                                           
072500     .                                                                    
072600     EJECT                                                                
072610 IMS-GNP-WDN612 SECTION.                                                  
072620                                                                          
072650     MOVE 'WDN612  '    TO SSA1                                           
072660     MOVE '  GE' TO GODK-STATUSKODER                                      
072670     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN612 SSA1                   
072680     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
072690     PERFORM IMS-STATUSKONTROLL                                           
072691     .                                                                    
072692     EJECT                                                                
074700 IMS-GU-WDD3BSEQ-TEXT SECTION.                                            
074800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
074900            DELIMITED BY SIZE INTO SSA1                                   
075000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
075100            DELIMITED BY SIZE INTO SSA2                                   
075200     MOVE '  GE' TO GODK-STATUSKODER                                      
075300     CALL CBLTDLI USING GU WDD3B-PCB DLI-IO-WDD311 SSA1 SSA2              
075400     MOVE WDD3B-STATUS-CODE TO STATUS-WS                                  
075500     PERFORM IMS-STATUSKONTROLL                                           
075600     SKIP3                                                                
075700     .                                                                    
075800 IMS-GU-WDD301-ASEQ SECTION.                                              
075900     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X  W-BEART-X ')'              
076100            DELIMITED BY SIZE INTO SSA1                                   
076200     MOVE '  GE' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GU WDD3A-PCB DLI-IO-WDD301 SSA1                   
076400     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     SKIP3                                                                
076700     .                                                                    
076800 IMS-GNP-WDD311      SECTION.                                             
076900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
077000            DELIMITED BY SIZE INTO SSA1                                   
077100     MOVE '  ' TO GODK-STATUSKODER                                        
077200     CALL CBLTDLI USING GNP WDD3A-PCB DLI-IO-WDD311 SSA1                  
077300     MOVE WDD3A-STATUS-CODE TO STATUS-WS                                  
077400     PERFORM IMS-STATUSKONTROLL                                           
077500     .                                                                    
077600     EJECT                                                                
077700 IMS-STATUSKONTROLL SECTION.                                              
077800                                                                          
077900     SET STATUS-IX TO 1                                                   
078000     SEARCH GODK-STATUS                                                   
078100       AT END                                                             
078200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
078300         DELIMITED BY SIZE INTO FELTEXT                                   
078400         CALL FELLOG                                                      
078500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078600         CONTINUE                                                         
078700     END-SEARCH                                                           
078800     .                                                                    
