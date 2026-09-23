001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W6016700.                                                
001500 AUTHOR.         BODIL LINDAHL.                                           
001600 DATE-WRITTEN.   98/05/11.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        UPPDATERING ETIKETTDATABAS FÖR BARCODE.                          
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WLETIA (WDK3)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W6T167                                              
002600*        MID:         W6I16701                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W6O16701                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6016700'.            
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004210 77  TEXT-IX                     PIC S9(5)   VALUE ZERO COMP-3.           
004220 77  TEXT-IX-MAX                 PIC S9(5)   VALUE +12  COMP-3.           
004230 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004300                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004802     88  INDATA-OK                           VALUE 'J'.                   
004810     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '6167'.                
005600     88  GODK-MID                            VALUE '6166' '6167'          
005700                                                   '6168'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007502     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007503     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W6I16701                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W6O16701                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001     03  W-IDARTNR-X.                                                     
011002         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011003     03  W-IDRADNR-X.                                                     
011010         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLETIA01'.                    
013002 01  DLI-IO-WLETIA01.                                                     
013003*    03  -COPY WDK301                                                     
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLETIA11'.                    
013006 01  DLI-IO-WLETIA11.                                                     
013010*    03  -COPY WDK311                                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013510     EJECT                                                                
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013802     EJECT                                                                
013803*01  -COPY W0008   -PRE ETIA-                                             
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ETIA-PCB.                     
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ETIA-PCB.                     
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014801         IF MFS-UPDATE                                                    
014802           PERFORM G-KOLLA-INPUT                                          
014803           IF INDATA-OK                                                   
014804             PERFORM H-UPPDATERA                                          
014805           END-IF                                                         
014810         ELSE                                                             
015001           IF MFS-FIRST                                                   
015002             PERFORM C-FOERSTA-SIDA                                       
015003           ELSE                                                           
015004             PERFORM E-SAMMA-SIDA                                         
015010           END-IF                                                         
015110         END-IF                                                           
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16701 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I16701                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I16701                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W6O167N1' TO MFS-IDMOD                                         
018300     MOVE '6167' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018410                                                                          
018420     IF MSGI-IDLAND-SPR = 'SE'                                            
018430        MOVE '0' TO MFS-KDHUVOMR                                          
018440     END-IF                                                               
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019200                                                                          
019300     ACCEPT DAGENS-DATUM FROM DATE                                        
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '6167'            TO MSGI-IDTRANS                               
020300     IF GODK-MID                                                          
020410        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700                                                                          
020800     MOVE JA TO NYCKLAR-SW                                                
021002                                                                          
021003     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021005     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021006       MOVE '7'         TO MFS-IDPFK                                      
021007       MOVE SPACE       TO MFS-KDTRTYP                                    
021008     END-IF                                                               
021009     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021010     IF MSGI-IDARTNR NUMERIC                                              
021011       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
021012     ELSE                                                                 
021013       MOVE NEJ TO NYCKLAR-SW                                             
021020     END-IF                                                               
021021                                                                          
021030     IF MSGI-IDLAND-SPR = 'SE'                                            
021040       MOVE 'S  ' TO MED-IDSKYLT                                          
021041     ELSE                                                                 
021042        MOVE'GB '  TO MED-IDSKYLT                                         
021050     END-IF                                                               
021101                                                                          
021102     IF GODK-MID OR NYCKLAR-OK                                            
021103       MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                
021104       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021105     ELSE                                                                 
021106       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
021110     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
022000     .                                                                    
022200     EJECT                                                                
022301 C-FOERSTA-SIDA SECTION.                                                  
022302                                                                          
022303     PERFORM MFS-RENSA-FAELT-IN                                           
022304     .                                                                    
022305     EJECT                                                                
022306 E-SAMMA-SIDA SECTION.                                                    
022307                                                                          
022309     IF EGEN-MID OR HELP-MID                                              
022310       IF MID-TEETIK-EXT(1)   = ALL '+'                                   
022311       AND MID-TEETIK-EXT(2)  = ALL '+'                                   
022312       AND MID-TEETIK-EXT(3)  = ALL '+'                                   
022313       AND MID-TEETIK-EXT(4)  = ALL '+'                                   
022314       AND MID-TEETIK-EXT(5)  = ALL '+'                                   
022315       AND MID-TEETIK-EXT(6)  = ALL '+'                                   
022316       AND MID-TEETIK-EXT(7)  = ALL '+'                                   
022317       AND MID-TEETIK-EXT(8)  = ALL '+'                                   
022318       AND MID-TEETIK-EXT(9)  = ALL '+'                                   
022319       AND MID-TEETIK-EXT(10) = ALL '+'                                   
022320       AND MID-TEETIK-EXT(11) = ALL '+'                                   
022321       AND MID-TEETIK-EXT(12) = ALL '+'                                   
022322         PERFORM MFS-RENSA-FAELT-IN                                       
022323       ELSE                                                               
022324         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022325         CALL WMEDKONV USING MED-WMEDAREA                                 
022326         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
022327         PERFORM EA-MID-INDATA-TILL-MOD                                   
022329       END-IF                                                             
022330     ELSE                                                                 
022331       PERFORM MFS-RENSA-FAELT-IN                                         
022332     END-IF                                                               
022333     .                                                                    
022334     EJECT                                                                
022335 EA-MID-INDATA-TILL-MOD SECTION.                                          
022336                                                                          
022339     MOVE +1 TO TEXT-IX                                                   
022340     PERFORM UNTIL TEXT-IX > TEXT-IX-MAX                                  
022341      IF MID-TEETIK-EXT(TEXT-IX) NOT = ALL '+'                            
022342        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEETIK-EXT-ATTR(TEXT-IX)        
022343        MOVE MFS-ROER-EJ-FAELT     TO MOD-TEETIK-EXT(TEXT-IX)             
022344      END-IF                                                              
022345      ADD +1 TO TEXT-IX                                                   
022346     END-PERFORM                                                          
022347     .                                                                    
022350     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022600     PERFORM FA-LAES-GRUNDDATA                                            
022700                                                                          
022800     IF SEGMENT-SAKNAS                                                    
022900        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023200        PERFORM MFS-RENSA-FAELT-UT                                        
023300     ELSE                                                                 
023400        PERFORM FB-VISA-RADINF                                            
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 FA-LAES-GRUNDDATA SECTION.                                               
023900                                                                          
024000     PERFORM IMS-GET-ETIA01                                               
024400     .                                                                    
024601     EJECT                                                                
024602 FB-VISA-RADINF SECTION.                                                  
024603                                                                          
024604     MOVE ETI-DAREGDAT TO MOD-DAREGDAT                                    
024605     MOVE ETI-TIUPPDAT TO MOD-TIUPPDAT                                    
024606     MOVE ETI-IDUSER   TO MOD-IDUSER                                      
024607                                                                          
024608     PERFORM IMS-GET-ETIA11                                               
024609     PERFORM UNTIL SEGMENT-SAKNAS                                         
024611        MOVE SPEC-IDRADNR TO TEXT-IX                                      
024612        IF EGEN-MID                                                       
024613           IF MID-TEETIK-EXT(TEXT-IX) = ALL '+'                           
024614              MOVE SPEC-TEETIK-EXT TO MOD-TEETIK-EXT(TEXT-IX)             
024615           END-IF                                                         
024616        ELSE                                                              
024617           MOVE SPEC-TEETIK-EXT TO MOD-TEETIK-EXT(TEXT-IX)                
024618        END-IF                                                            
024619        PERFORM IMS-GET-ETIA11                                            
024620     END-PERFORM                                                          
024621     .                                                                    
024622     EJECT                                                                
024623 G-KOLLA-INPUT SECTION.                                                   
024624                                                                          
024625     MOVE JA TO INDATA-SW                                                 
024626                                                                          
024627     IF MID-TEETIK-EXT(1)   = ALL '+'                                     
024628     AND MID-TEETIK-EXT(2)  = ALL '+'                                     
024629     AND MID-TEETIK-EXT(3)  = ALL '+'                                     
024630     AND MID-TEETIK-EXT(4)  = ALL '+'                                     
024631     AND MID-TEETIK-EXT(5)  = ALL '+'                                     
024632     AND MID-TEETIK-EXT(6)  = ALL '+'                                     
024633     AND MID-TEETIK-EXT(7)  = ALL '+'                                     
024634     AND MID-TEETIK-EXT(8)  = ALL '+'                                     
024635     AND MID-TEETIK-EXT(9)  = ALL '+'                                     
024636     AND MID-TEETIK-EXT(10) = ALL '+'                                     
024637     AND MID-TEETIK-EXT(11) = ALL '+'                                     
024638     AND MID-TEETIK-EXT(12) = ALL '+'                                     
024639       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024640       CALL WMEDKONV USING MED-WMEDAREA                                   
024641       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024642       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024643       MOVE NEJ TO INDATA-SW                                              
024644     ELSE                                                                 
024645                                                                          
024646        MOVE +1 TO TEXT-IX                                                
024647        PERFORM UNTIL TEXT-IX > TEXT-IX-MAX                               
024648          IF MID-TEETIK-EXT(TEXT-IX) NOT = ALL '+'                        
024652             MOVE MFS-ALFA-FAELT-RAETT                                    
024653                         TO MOD-TEETIK-EXT-ATTR(TEXT-IX)                  
024654          END-IF                                                          
024674          ADD +1 TO TEXT-IX                                               
024675        END-PERFORM                                                       
024676                                                                          
024677        IF INDATA-FEL                                                     
024678          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
024679          CALL WMEDKONV USING MED-WMEDAREA                                
024680          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
024681          PERFORM MFS-ROER-EJ-FAELT-UT                                    
024693        END-IF                                                            
024694     END-IF                                                               
024695     .                                                                    
024696     EJECT                                                                
024697 H-UPPDATERA SECTION.                                                     
024698                                                                          
024699     PERFORM IMS-GET-ETIA01                                               
024700                                                                          
024701     IF SEGMENT-FINNS                                                     
024703       MOVE +1 TO TEXT-IX                                                 
024704                  W-IDRADNR                                               
024705       PERFORM IMS-GHU-ETIA11                                             
024706                                                                          
024707       PERFORM UNTIL TEXT-IX > TEXT-IX-MAX                                
024709          IF SEGMENT-FINNS                                                
024711            IF MID-TEETIK-EXT(TEXT-IX) NOT = ALL '+'                      
024712               IF MID-TEETIK-EXT(TEXT-IX) = SPACE                         
024713                  PERFORM IMS-DLET-ETIA                                   
024714               ELSE                                                       
024715                  MOVE MID-TEETIK-EXT(TEXT-IX)                            
024716                                       TO SPEC-TEETIK-EXT                 
024717                                          MOD-TEETIK-EXT(TEXT-IX)         
024718                  MOVE MFS-ADD-LYS-UPP-FAELT                              
024719                                  TO MOD-TEETIK-EXT-ATTR(TEXT-IX)         
024720                                                                          
024721                  PERFORM IMS-REPL-ETIA11                                 
024722               END-IF                                                     
024723            END-IF                                                        
024724          ELSE                                                            
024725            IF MID-TEETIK-EXT(TEXT-IX) NOT = ALL '+'                      
024726               MOVE MID-TEETIK-EXT(TEXT-IX)                               
024727                                    TO SPEC-TEETIK-EXT                    
024728                                       MOD-TEETIK-EXT(TEXT-IX)            
024729               MOVE MFS-ADD-LYS-UPP-FAELT                                 
024730                               TO MOD-TEETIK-EXT-ATTR(TEXT-IX)            
024731               MOVE TEXT-IX        TO SPEC-IDRADNR                        
024732               PERFORM IMS-ISRT-ETIA11                                    
024733            END-IF                                                        
024734          END-IF                                                          
024735          ADD +1 TO TEXT-IX                                               
024736                    W-IDRADNR                                             
024737          PERFORM IMS-GHU-ETIA11                                          
024738       END-PERFORM                                                        
024739                                                                          
024740       PERFORM IMS-GET-ETIA01                                             
024741       MOVE DAGENS-DATUM      TO ETI-TIUPPDAT                             
024742       MOVE MSG-SIGNON-USERID TO ETI-IDUSER                               
024743       PERFORM IMS-REPL-ETIA01                                            
024744                                                                          
024745       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
024746       CALL WMEDKONV USING MED-WMEDAREA                                   
024747       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
024748     END-IF                                                               
024749     .                                                                    
024750     EJECT                                                                
024800 MFS-RENSA-FAELT-IN SECTION.                                              
024900                                                                          
025000     MOVE +1 TO TEXT-IX                                                   
025100     PERFORM UNTIL TEXT-IX > TEXT-IX-MAX                                  
025200        MOVE MFS-RENSA-FAELT TO MOD-TEETIK-EXT(TEXT-IX)                   
025300        ADD +1 TO TEXT-IX                                                 
025310     END-PERFORM                                                          
025400     .                                                                    
025600     SKIP3                                                                
025700 MFS-RENSA-FAELT-UT SECTION.                                              
025800                                                                          
025900     MOVE +1 TO TEXT-IX                                                   
026000     PERFORM UNTIL TEXT-IX > TEXT-IX-MAX                                  
026100        MOVE MFS-RENSA-FAELT TO MOD-TEETIK-EXT(TEXT-IX)                   
026200        ADD +1 TO TEXT-IX                                                 
026300     END-PERFORM                                                          
026301     MOVE MFS-RENSA-FAELT TO MOD-IDUSER                                   
026302                             MOD-TIUPPDAT                                 
026303                             MOD-DAREGDAT                                 
026310     .                                                                    
026320     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500                                                                          
026700     MOVE +1 TO TEXT-IX                                                   
026710     PERFORM UNTIL TEXT-IX > TEXT-IX-MAX                                  
026720        MOVE MFS-ROER-EJ-FAELT TO MOD-TEETIK-EXT(TEXT-IX)                 
026730        ADD +1 TO TEXT-IX                                                 
026740     END-PERFORM                                                          
026750     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER                                 
026760                               MOD-TIUPPDAT                               
026770                               MOD-DAREGDAT                               
027100     .                                                                    
027200     SKIP3                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400                                                                          
027410     MOVE +1 TO TEXT-IX                                                   
027420     PERFORM UNTIL TEXT-IX > TEXT-IX-MAX                                  
027430        MOVE MFS-ROER-EJ-FAELT TO MOD-TEETIK-EXT(TEXT-IX)                 
027440        ADD +1 TO TEXT-IX                                                 
027450     END-PERFORM                                                          
027800     .                                                                    
027900     EJECT                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030600     IF MSGI-IDLAND-SPR = 'SE'                                            
030700       MOVE '0' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GET-ETIA01 SECTION.                                                  
031504     STRING 'WLETIA01(IDARTNR  =' W-IDARTNR-X ')'                         
031505          DELIMITED BY SIZE INTO SSA1                                     
031506     MOVE '  GE' TO GODK-STATUSKODER                                      
031507     CALL CBLTDLI USING GHU ETIA-PCB DLI-IO-WLETIA01 SSA1                 
031508     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
031509     PERFORM IMS-STATUSKONTROLL                                           
031510     .                                                                    
031511     SKIP3                                                                
031512 IMS-GET-ETIA11 SECTION.                                                  
031515     MOVE 'WLETIA11 ' TO SSA1                                             
031516     MOVE '  GE' TO GODK-STATUSKODER                                      
031517     CALL CBLTDLI USING GNP ETIA-PCB DLI-IO-WLETIA11 SSA1                 
031518     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
031519     PERFORM IMS-STATUSKONTROLL                                           
031520     .                                                                    
031521     SKIP3                                                                
031522 IMS-GHU-ETIA11 SECTION.                                                  
031523     STRING 'WLETIA01(IDARTNR  =' W-IDARTNR-X ')'                         
031524          DELIMITED BY SIZE INTO SSA1                                     
031525     STRING 'WLETIA11(IDRADNR  =' W-IDRADNR-X ')'                         
031526          DELIMITED BY SIZE INTO SSA2                                     
031527     MOVE '  GE' TO GODK-STATUSKODER                                      
031528     CALL CBLTDLI USING GHNP ETIA-PCB DLI-IO-WLETIA11 SSA1 SSA2           
031529     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
031530     PERFORM IMS-STATUSKONTROLL                                           
031531     .                                                                    
031532     EJECT                                                                
031533 IMS-REPL-ETIA01 SECTION.                                                 
031534     MOVE '  ' TO GODK-STATUSKODER                                        
031535     CALL CBLTDLI USING REPL ETIA-PCB DLI-IO-WLETIA01                     
031536     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
031537     PERFORM IMS-STATUSKONTROLL                                           
031538     .                                                                    
031539     SKIP3                                                                
031540 IMS-REPL-ETIA11 SECTION.                                                 
031541     MOVE '  ' TO GODK-STATUSKODER                                        
031542     CALL CBLTDLI USING REPL ETIA-PCB DLI-IO-WLETIA11                     
031543     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
031544     PERFORM IMS-STATUSKONTROLL                                           
031545     .                                                                    
031555     SKIP3                                                                
031556 IMS-ISRT-ETIA11 SECTION.                                                 
031558     STRING 'WLETIA01(IDARTNR  =' W-IDARTNR-X ')'                         
031559          DELIMITED BY SIZE INTO SSA1                                     
031560     MOVE 'WLETIA11 ' TO SSA2                                             
031561     MOVE '  ' TO GODK-STATUSKODER                                        
031562     CALL CBLTDLI USING ISRT ETIA-PCB DLI-IO-WLETIA11 SSA1 SSA2           
031563     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
031564     PERFORM IMS-STATUSKONTROLL                                           
031565     .                                                                    
031600     EJECT                                                                
031610 IMS-DLET-ETIA SECTION.                                                   
031620     MOVE '  ' TO GODK-STATUSKODER                                        
031630     CALL CBLTDLI USING DLET ETIA-PCB DLI-IO-WLETIA11                     
031640     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
031650     PERFORM IMS-STATUSKONTROLL                                           
031660     .                                                                    
031670     SKIP3                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
