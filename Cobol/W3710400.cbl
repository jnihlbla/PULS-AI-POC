001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W3710400.                                                
001300 AUTHOR.         RANDI BERG.                                              
001400 DATE-WRITTEN.   98/06/15.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*        SKAPAR EN FIL MED AVVIKELSER MELLAN LEVERERAT                    
002000*        ANTAL OCH MOTTAGET ANTAL FRÅN WDR4.                              
002010*        (SYMBOLISK PARAMETER IN-IDFAKT/IDDC/IDKOLLI)                     
002101*                                                                         
002110*        PROGRAMMET LÄSER    WDR4                                         
002200*                                                                         
002210*        ÄNDRAD 050516/EÖ ETRACKER 1863643                                
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003001     SKIP2                                                                
003005*          --- IDFAKT PARAMETER FRÅN W3018400                             
003006     SELECT INDATA                     ASSIGN TO W37104D1.                
003007     SKIP2                                                                
003008*          --- UTFIL                                                      
003009     SELECT W3717B                     ASSIGN TO W37104D2.                
003230                                                                          
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003710     SKIP3                                                                
003720 FD  INDATA                                                               
003730     RECORDING       F                                                    
003740     BLOCK CONTAINS  0.                                                   
003750     SKIP2                                                                
003760 01  INPOST              PIC X(80).                                       
003761     SKIP3                                                                
003762 FD  W3717B                                                               
003763     RECORDING       F                                                    
003764     BLOCK CONTAINS  0.                                                   
003765     SKIP2                                                                
003766*01  POST -COPY W3717B01 -PRE  UT1-  -L.                                  
003780     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W3710400'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     SKIP2                                                                
004310*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
004320 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
004330 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
004331                                                                          
004350     SKIP2                                                                
004400 01  FELTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100     EJECT                                                                
005710                                                                          
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900*                                                                         
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006301     EJECT                                                                
006302*    --- PARAMETRAR TILL POSTSUM                                          
006303*                                                                         
006310*01  -COPY W0005   -PRE  POSTSUM-                                         
006601     EJECT                                                                
006602*                                                                         
006611 01  IN-AREA-START               PIC X(16)   VALUE                        
006612                                 'IN-AREA-START  '.                       
006613     SKIP2                                                                
006614 01  IN-AREA.                                                             
006615     03  IN-IDDC                 PIC X(2).                                
006616     03  FILLER                  PIC X.                                   
006617     03  IN-IDFAKT               PIC 9(7).                                
006618     03  FILLER                  PIC X.                                   
006619     03  IN-IDKOLLI              PIC 9(5).                                
006620     EJECT                                                                
006621                                                                          
006627 01  UT1-AREA-START              PIC X(24)   VALUE                        
006628                                 'UT1-AREA-START'.                        
006629     SKIP2                                                                
006630                                                                          
006631*01  AREA -COPY W3717B01   -PRE UT1-                                      
006632                                                                          
006640     EJECT                                                                
006900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007000     SKIP3                                                                
007100 01  NYCKLAR-TILL-DLI.                                                    
007110     03   W-WDGXKEY-3171-X.                                               
007120         05  W-IDHTYP            PIC X(4)   VALUE '3171'.                 
007130         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
007140                                                                          
007197     03  W-IDFAKT-X.                                                      
007198         05 W-IDFAKT             PIC S9(7) VALUE ZERO COMP-3.             
007199                                                                          
007218     03  W-IDKOLLI-MIN-X.                                                 
007219         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
007220                                                                          
007230     03  W-IDKOLLI-MAX-X.                                                 
007240         05  W-IDKOLLI-MAX       PIC S9(5)   VALUE 99999 COMP-3.          
007241                                                                          
007242     03  W-KDTRSTAT-X.                                                    
007243         05 W-KDTRSTAT           PIC S9    VALUE +0 COMP-3.               
007244                                                                          
007250     03  W-IDARTNR-X.                                                     
007260         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
007300     EJECT                                                                
007400*    --- STATUS-KOD FRÅN IMS                                              
007500 01  STATUS-WS                   PIC XX.                                  
007600     88  SEGMENT-FINNS                       VALUE '  '.                  
007800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008100     SKIP2                                                                
008200 01  GODK-STATUSKODER.                                                    
008300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008400     SKIP3                                                                
008500 01  SSA1                        PIC X(64).                               
008600 01  SSA2                        PIC X(64).                               
008700     EJECT                                                                
008800*    --- IMS FUNKTIONSKODER                                               
008900*01  -COPY W0003                                                          
009000     EJECT                                                                
009200*    ---  DLI INPUT-OUTPUT AREA                                           
009301 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3172'.               
009302 01  DLI-IO-3172.                                                         
009303*    03  -COPY WDGX3172                                                   
009304     EJECT                                                                
009305 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3174'.               
009306 01  DLI-IO-3174.                                                         
009307*    03  -COPY WDGX3174                                                   
009308     EJECT                                                                
009309 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3176'.               
009310 01  DLI-IO-3176.                                                         
009311*    03  -COPY WDGX3176                                                   
009312     EJECT                                                                
009800 LINKAGE SECTION.                                                         
009900                                                                          
010130*01  -COPY W0008  -PRE 3171-                                              
010140     05  FILLER                  PIC X.                                   
010400     EJECT                                                                
010503 PROCEDURE DIVISION  USING 3171-PCB.                                      
010504 MAIN SECTION.                                                            
010510     ENTRY 'DLITCBL' USING 3171-PCB.                                      
010600                                                                          
010800     SKIP2                                                                
010900     PERFORM A-INIT                                                       
010910                                                                          
010920     MOVE IN-IDFAKT         TO W-IDFAKT                                   
011010     PERFORM IMS-GU-WDGX3172                                              
011013                                                                          
011015     MOVE IN-IDKOLLI        TO W-IDKOLLI-MIN                              
011016                               W-IDKOLLI-MAX                              
011025     MOVE +4                TO W-KDTRSTAT                                 
011027     PERFORM IMS-GNP-WDGX3174-ST4                                         
011030     IF SEGMENT-FINNS                                                     
011031                                                                          
011080       PERFORM IMS-GNP-WDGX3176                                           
011100       PERFORM UNTIL SEGMENT-SAKNAS                                       
011200                                                                          
011207         IF 3176-KVANTAL-DEB NOT = 3176-KVANTMOT                          
011209           PERFORM B-SKAPA-FILER                                          
011210         END-IF                                                           
011220                                                                          
011241         PERFORM IMS-GNP-WDGX3176                                         
011503       END-PERFORM                                                        
011507                                                                          
011530     END-IF                                                               
012300                                                                          
012310     PERFORM Z-FINIT                                                      
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
012800 A-INIT SECTION.                                                          
013101                                                                          
013102     OPEN INPUT INDATA                                                    
013103     READ INDATA INTO IN-AREA                                             
013104     END-READ                                                             
013105                                                                          
013111     CLOSE INDATA                                                         
013112                                                                          
013113     OPEN OUTPUT W3717B                                                   
013120                                                                          
013610     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
013620     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
013630     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
013900     .                                                                    
014000     EJECT                                                                
014010 B-SKAPA-FILER SECTION.                                                   
014020                                                                          
014030     MOVE 3172-IDFAKT           TO UT1-IDFAKT                             
014031     MOVE 3172-IDDC-SEND        TO UT1-IDDC                               
014040     MOVE 3176-IDARTNR-OBJ      TO UT1-IDARTNR                            
014041     MOVE 3176-KVANTAL-DEB      TO UT1-KVLEVART                           
014042     MOVE 3176-KVANTMOT         TO UT1-KVANTMOT                           
014070     COMPUTE UT1-KVAVV-KVANT = 3176-KVANTAL-DEB -                         
014080                               3176-KVANTMOT                              
014090     PERFORM S11-SKRIV-W3717B                                             
014170     .                                                                    
014171     EJECT                                                                
014180 Z-FINIT SECTION.                                                         
014200                                                                          
014303     CLOSE W3717B                                                         
014305                                                                          
014501     SKIP2                                                                
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014901     EJECT                                                                
014904 S11-SKRIV-W3717B SECTION.                                                
014905     SKIP2                                                                
014908     WRITE UT1-POST FROM UT1-AREA                                         
014909                                                                          
014910     MOVE 'W3717B ' TO POSTSUM-FDNAMN                                     
014911     MOVE 'W37104D2' TO POSTSUM-DDNAMN2                                   
014912     CALL POSTSUM USING POSTSUM-PARM                                      
014913     .                                                                    
014914     EJECT                                                                
015200* --- IMS SEKTIONER ---                                                   
015300                                                                          
015400 IMS-GU-WDGX3172 SECTION.                                                 
015401                                                                          
015402     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
015403          DELIMITED BY SIZE INTO SSA1                                     
015404     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-X ')'                          
015405          DELIMITED BY SIZE INTO SSA2                                     
015406     MOVE '  ' TO GODK-STATUSKODER                                        
015407     CALL CBLTDLI USING GU 3171-PCB DLI-IO-3172 SSA1 SSA2                 
015408     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
015409     PERFORM IMS-STATUSKONTROLL                                           
015410     .                                                                    
015411     SKIP3                                                                
015420 IMS-GNP-WDGX3174-ST4 SECTION.                                            
015421                                                                          
015422     STRING 'WDGX3174(IDKOLLI >=' W-IDKOLLI-MIN-X                         
015423                    '&IDKOLLI <=' W-IDKOLLI-MAX-X                         
015424                    '&KDTRSTAT =' W-KDTRSTAT-X ')'                        
015425          DELIMITED BY SIZE INTO SSA1                                     
015426     MOVE '  GE' TO GODK-STATUSKODER                                      
015427     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-3174 SSA1                     
015428     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
015429     PERFORM IMS-STATUSKONTROLL                                           
015430     .                                                                    
015431     SKIP2                                                                
015452 IMS-GNP-WDGX3176 SECTION.                                                
015453                                                                          
015454     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-MIN-X ')'                     
015455          DELIMITED BY SIZE INTO SSA1                                     
015456     MOVE 'WDGX3176' TO SSA2                                              
015457     MOVE '  GE' TO GODK-STATUSKODER                                      
015458     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-3176 SSA1 SSA2                
015459     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
015460     PERFORM IMS-STATUSKONTROLL                                           
015461     .                                                                    
015466     SKIP2                                                                
015620 IMS-STATUSKONTROLL SECTION.                                              
015700     SKIP2                                                                
015800     SET STATUS-IX TO 1                                                   
015900     SEARCH GODK-STATUS                                                   
016000       AT END                                                             
016100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016200           DELIMITED BY SIZE INTO FELTEXT                                 
016300         DISPLAY FELTEXT                                                  
016400         CALL FELLOG                                                      
016500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016600         CONTINUE                                                         
016700     END-SEARCH                                                           
016800     .                                                                    
