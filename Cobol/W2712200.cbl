000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2712200.                                                
000400*AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500*DATE-WRITTEN.   98/12/09.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR LOKALA LEVERANTÖRER (JAPAN)                           
001100*        INNEHÅLLER JAPANSKA SOM ANVÄNDS FÖR UTSKRIFT AV                  
001200*        PURCHASE ORDER W2711800                                          
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLLEVA (WDF1)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- JAPANSKA ADRESSER                                          
003000     SELECT W27122                     ASSIGN TO W27122D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W27122                                                               
003610     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W27122      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2712200'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004620 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004621 77  WS-ANTAL-WDK711             PIC S9(9)   VALUE ZERO COMP-3.           
004700     SKIP2                                                                
004701 01  CHKP-VAR.                                                            
004702 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004703 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004704 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004705 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004706 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004710 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004720                                                                          
004815                                                                          
004820 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100                                                                          
005200 77  W27122-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W27122                       VALUE 'J'.                   
005301                                                                          
005310 77  K7-SW                       PIC X       VALUE 'J'.                   
005320     88  K7-FINNS                            VALUE 'J'.                   
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                             'IN-AREA-START'.             
007300     SKIP2                                                                
007400                                                                          
007500*01  AREA -COPY W27122     -PRE IN-                                       
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-IDARTNR-X.                                                     
008200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     03  W-IDDC-X.                                                        
008400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008420     03  W-IDLEVNR-X.                                                     
008430         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
008460     03  W-KDSEGKEY-X.                                                    
008470         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010210*    ---  DLI INPUT-OUTPUT AREA                                           
010220                                                                          
010230 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLLEVA01'.                    
010240 01  DLI-IO-WLLEVA01.                                                     
010250*    03  -COPY WDF101                                                     
010260     EJECT                                                                
010292 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLLEVA17'.                    
010293 01  DLI-IO-WLLEVA17.                                                     
010294*    03  -COPY WDF117                                                     
011400     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W0009   -PRE MSG-                                              
011800     EJECT                                                                
011900*01  -COPY W0008  -PRE LEVA-                                              
012000     05  FILLER                  PIC X.                                   
012100     EJECT                                                                
012200 PROCEDURE DIVISION  USING MSG-PCB LEVA-PCB.                              
012300     ENTRY 'DLITCBL' USING MSG-PCB LEVA-PCB.                              
012400                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM S01-LAES-W27122                                              
012710     PERFORM UNTIL END-OF-W27122                                          
012720                                                                          
012793       PERFORM B-BEHANDLA-POSTER                                          
012796       PERFORM S01-LAES-W27122                                            
012797                                                                          
012798     END-PERFORM                                                          
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900     SKIP2                                                                
014000                                                                          
014001     ACCEPT DAGENS-DATUM FROM DATE                                        
014010                                                                          
014100     OPEN INPUT W27122                                                    
014300                                                                          
014400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014500     .                                                                    
014600     EJECT                                                                
014700 B-BEHANDLA-POSTER SECTION.                                               
014800                                                                          
014801     MOVE IN-IDLEVNR TO W-IDLEVNR                                         
014810     MOVE ZERO TO TALLY                                                   
014820     INSPECT IN-IDLEVNR TALLYING TALLY FOR LEADING ZEROES                 
014830     IF TALLY = 5                                                         
014840        MOVE SPACE TO W-IDLEVNR                                           
014850     ELSE                                                                 
014860        MOVE IN-IDLEVNR (TALLY + 1:) TO W-IDLEVNR                         
014870     END-IF                                                               
015100     PERFORM IMS-GU-LEVA-WDF101                                           
015101     IF SEGMENT-SAKNAS                                                    
015112                                                                          
015113       DISPLAY 'LEVERANTÖR FINNS EJ UPPLAGD : ' IN-IDLEVNR                
015114     ELSE                                                                 
015120       PERFORM IMS-GHU-LEVA-WDF117                                        
015127       IF SEGMENT-FINNS                                                   
015128         MOVE SPACE          TO JPN-WDF117                                
015129         MOVE '1'            TO JPN-KDSEGKEY                              
015130         MOVE IN-BELEV       TO JPN-BELEV                                 
015131         MOVE IN-ADLEV-RAD1  TO JPN-ADLEV-RAD1                            
015132         MOVE IN-ADLEV-RAD2  TO JPN-ADLEV-RAD2                            
015133         MOVE IN-ADLEV-POSTNR                                             
015134                             TO JPN-ADLEV-ORT (1:8)                       
015135         MOVE IN-ADLEV-ORT   TO JPN-ADLEV-ORT (9:20)                      
015150         PERFORM IMS-REPL-LEVA-WDF117                                     
015198       ELSE                                                               
015199         MOVE SPACE          TO JPN-WDF117                                
015200         MOVE '1'            TO JPN-KDSEGKEY                              
015201         MOVE IN-BELEV       TO JPN-BELEV                                 
015202         MOVE IN-ADLEV-RAD1  TO JPN-ADLEV-RAD1                            
015203         MOVE IN-ADLEV-RAD2  TO JPN-ADLEV-RAD2                            
015204         MOVE IN-ADLEV-POSTNR                                             
015205                             TO JPN-ADLEV-ORT (1:8)                       
015206         MOVE IN-ADLEV-ORT   TO JPN-ADLEV-ORT (9:20)                      
015207         PERFORM IMS-ISRT-LEVA-WDF117                                     
015210       END-IF                                                             
015220     END-IF                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W27122                                                         
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W27122  SECTION.                                                
016500     SKIP2                                                                
016600     READ W27122 INTO IN-AREA                                             
016700     AT END                                                               
016900        SET END-OF-W27122 TO TRUE                                         
017000                                                                          
017100     NOT AT END                                                           
017200        MOVE 'W27122' TO POSTSUM-FDNAMN                                   
017300        MOVE 'W27122D1' TO POSTSUM-DDNAMN2                                
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017900 IMS-GU-LEVA-WDF101 SECTION.                                              
018010     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
018100          DELIMITED BY SIZE INTO SSA1                                     
018110     MOVE '  GE' TO GODK-STATUSKODER                                      
018120     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-WLLEVA01 SSA1                  
018130     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
018140     PERFORM IMS-STATUSKONTROLL                                           
018150     .                                                                    
018160     EJECT                                                                
018200 IMS-GHU-LEVA-WDF117 SECTION.                                             
020920                                                                          
020921     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
020922          DELIMITED BY SIZE INTO SSA1                                     
020930     STRING 'WLLEVA17(KDSEGKEY =' W-KDSEGKEY-X ')'                        
020940          DELIMITED BY SIZE INTO SSA2                                     
020950     MOVE '  GE' TO GODK-STATUSKODER                                      
020960     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-WLLEVA17 SSA1 SSA2            
020970     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
020980     PERFORM IMS-STATUSKONTROLL                                           
020990     .                                                                    
020991     SKIP3                                                                
020993 IMS-ISRT-LEVA-WDF101 SECTION.                                            
020994                                                                          
020995     MOVE 'WLLEVA01 ' TO SSA1                                             
020996     MOVE '  ' TO GODK-STATUSKODER                                        
020997     CALL CBLTDLI USING ISRT LEVA-PCB DLI-IO-WLLEVA01 SSA1                
020998     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
020999     PERFORM IMS-STATUSKONTROLL                                           
021000     .                                                                    
021001     SKIP3                                                                
021002 IMS-ISRT-LEVA-WDF117 SECTION.                                            
021003                                                                          
021004     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
021005          DELIMITED BY SIZE INTO SSA1                                     
021006     MOVE 'WLLEVA17 ' TO SSA2                                             
021007     MOVE '  ' TO GODK-STATUSKODER                                        
021008     CALL CBLTDLI USING ISRT LEVA-PCB DLI-IO-WLLEVA17 SSA1 SSA2           
021009     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
021010     PERFORM IMS-STATUSKONTROLL                                           
021011     .                                                                    
021012     SKIP3                                                                
021013 IMS-REPL-LEVA-WDF117 SECTION.                                            
021014                                                                          
021015     MOVE '  ' TO GODK-STATUSKODER                                        
021016     CALL CBLTDLI USING REPL LEVA-PCB DLI-IO-WLLEVA17                     
021017     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
021018     PERFORM IMS-STATUSKONTROLL                                           
021019     .                                                                    
021020     EJECT                                                                
021030 IMS-STATUSKONTROLL SECTION.                                              
021100     SKIP2                                                                
021200     SET STATUS-IX TO 1                                                   
021300     SEARCH GODK-STATUS                                                   
021400       AT END                                                             
021500         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
021600         DISPLAY FELTEXT                                                  
021700         CALL FELLOG                                                      
021800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021900         CONTINUE                                                         
022000     END-SEARCH                                                           
022100     .                                                                    
