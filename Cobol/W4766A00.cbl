000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4766A00.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   03/10/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER W47664 , SORTERADE POSTER                       
001100*        SKRIVS PÅ UTFILEN SAMT HÄMTAR PRAVCOST FÖR USA/CAN               
001200*                                                                         
001300*        PROGRAMMET LÄSER     WDK7 ARTIKELREG                             
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*     --- INPOSTER FRÅN W4766800                                          
002400     SELECT W47664                     ASSIGN TO W4766AD1.                
002500     SKIP2                                                                
002600*     --- UTFIL MED TRANS TILL REFILL-SYSTEM                              
002700     SELECT W4766A                     ASSIGN TO W4766AD2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W47664                                                               
003400     RECORDING       V                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W4766401     -L.                                               
003800     SKIP3                                                                
003900 FD  W4766A                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200*01  UT-POST   -COPY W4766401 -L.                                         
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77  IDPGM                       PIC X(8)    VALUE 'W4766A00'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  W47664-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W47664                       VALUE 'J'.                   
005501                                                                          
005502 77  W-POST                      PIC S9(1)   VALUE ZERO COMP-3.           
005510                                                                          
005511 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005512                                                                          
005513 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
005514 01  WS-SEKTION                  PIC X(32)   VALUE SPACE.                 
005515                                                                          
005516 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
005518 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
005519 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
005520 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
005521                                                                          
005522*    ----  VALID IDDC CODES                                               
005523*01    -COPY  WWDC99                                                      
005527     EJECT                                                                
005600                                                                          
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005810     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4) COMP VALUE +0.                 
006700     EJECT                                                                
007700                                                                          
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
008300                                                                          
008400 01  IN-AREA.                                                             
008500*    03  -COPY W4766401   -PRE IN-                                        
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
008800                                                                          
008900 01  UT-AREA.                                                             
009100*    03  -COPY W4766401   -PRE UT-                                        
010000     EJECT                                                                
010001 01  W-AREA.                                                              
010002*    03  -COPY W4766401                                                   
010003*    03  -COPY W4766402                                                   
010004*    03  -COPY W4766403                                                   
010010*                                                                         
010020 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010030     SKIP3                                                                
010040 01  NYCKLAR-TILL-DLI.                                                    
010050                                                                          
010107     03  W-IDARTNR-X.                                                     
010108         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010109                                                                          
010110     03  W-IDDC-X.                                                        
010111         05  W-IDDC              PIC XX      VALUE SPACE.                 
010112                                                                          
010113*    --- STATUS-KOD FRÅN IMS                                              
010114 01  STATUS-WS                   PIC XX.                                  
010115     88  SEGMENT-FINNS                       VALUE '  '.                  
010116     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010117     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010118     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010119     88  IMS-EJ-OK                           VALUE 'XD'.                  
010120     SKIP2                                                                
010121 01  GODK-STATUSKODER.                                                    
010122     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010123     SKIP3                                                                
010124 01  SSA1                        PIC X(64).                               
010125 01  SSA2                        PIC X(64).                               
010126     EJECT                                                                
010127*    --- IMS FUNKTIONSKODER                                               
010128*01  -COPY W0003                                                          
010129     EJECT                                                                
010130                                                                          
010131 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
010132 01  DLI-IO-WDK711.                                                       
010133*    03  -COPY WDK711                                                     
010134                                                                          
010135*01  -COPY W0009         -PRE MSG-                                        
010142 LINKAGE SECTION.                                                         
010144                                                                          
010145*01  -COPY W0008  -PRE  WDK7-                                             
010146     05  FILLER                  PIC X.                                   
010147                                                                          
010153                                                                          
010154 PROCEDURE DIVISION       USING                                           
010155                                 WDK7-PCB.                                
010200 MAIN SECTION.                                                            
010210     ENTRY 'DLITCBL'      USING                                           
010213                                 WDK7-PCB.                                
010220                                                                          
010300                                                                          
010400     PERFORM A-INIT                                                       
010500                                                                          
010600     PERFORM S01-LAES-W47664                                              
011200     PERFORM UNTIL END-OF-W47664                                          
011300                                                                          
011400       PERFORM D-TESTA-UPPDATERA-POSTER                                   
011500                                                                          
011600       PERFORM S01-LAES-W47664                                            
011700     END-PERFORM                                                          
011800                                                                          
011900     PERFORM Z-FINIT                                                      
012000                                                                          
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
012400     EJECT                                                                
012500 A-INIT SECTION.                                                          
012600                                                                          
012700     OPEN INPUT  W47664                                                   
012800     OPEN OUTPUT W4766A                                                   
012900                                                                          
013000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013010                                                                          
013100     .                                                                    
013200     EJECT                                                                
013300 D-TESTA-UPPDATERA-POSTER  SECTION.                                       
013400                                                                          
013500*    TRANS TILL REFILL-SYSTEM,                                            
013600                                                                          
014610     MOVE IN-001-IDPTYP          TO W-POST                                
014700                                                                          
014701     EVALUATE W-POST                                                      
014702       WHEN 1                                                             
014703         MOVE IN-AREA            TO 001-W4766401                          
014704       WHEN 2                                                             
014705         MOVE IN-AREA            TO 002-W4766402                          
014706       WHEN 3                                                             
014707         MOVE IN-AREA            TO 003-W4766403                          
014708         PERFORM DA-HAMTA-PRAVCOST                                        
014709     END-EVALUATE                                                         
014710                                                                          
014711     PERFORM S11-SKRIV-W4766A                                             
014712     .                                                                    
014713     EJECT                                                                
014714 DA-HAMTA-PRAVCOST  SECTION.                                              
014715                                                                          
014914     MOVE 001-IDDC               TO WS-IDDC                               
014915                                    W-IDDC                                
014920                                                                          
014930     MOVE 003-IDARTNR            TO W-IDARTNR                             
014960                                                                          
014970     PERFORM IMS-GU-WDK711                                                
014980     IF SEGMENT-FINNS                                                     
014990        MOVE SLAG-PRAVCOST       TO 003-PRAVCOST                          
015000     END-IF                                                               
015111                                                                          
015112     .                                                                    
015120     EJECT                                                                
015200 Z-FINIT SECTION.                                                         
015300                                                                          
015400     CLOSE W47664                                                         
015500           W4766A                                                         
015600                                                                          
015700     MOVE 'S' TO POSTSUM-OPKOD                                            
015800     CALL POSTSUM USING POSTSUM-PARM                                      
015900     .                                                                    
016000     EJECT                                                                
016100 S01-LAES-W47664  SECTION.                                                
016200                                                                          
016300     READ W47664 INTO IN-AREA                                             
016400     AT END                                                               
016500        SET END-OF-W47664 TO TRUE                                         
016600     NOT AT END                                                           
016700        MOVE 'W47664'       TO POSTSUM-FDNAMN                             
016800        MOVE 'W4766AD1'     TO POSTSUM-DDNAMN2                            
016900        MOVE SPACE          TO POSTSUM-TRANSTYP                           
017000        CALL POSTSUM USING POSTSUM-PARM                                   
017100     END-READ                                                             
017200     .                                                                    
017300     EJECT                                                                
018600 S11-SKRIV-W4766A SECTION.                                                
018700                                                                          
018710     EVALUATE W-POST                                                      
018720       WHEN 1                                                             
018730         WRITE UT-POST FROM 001-W4766401                                  
018740       WHEN 2                                                             
018750         WRITE UT-POST FROM 002-W4766402                                  
018760       WHEN 3                                                             
018770         WRITE UT-POST FROM 003-W4766403                                  
018780     END-EVALUATE                                                         
020000                                                                          
020100     MOVE W-POST            TO POSTSUM-TRANSTYP                           
020200     MOVE 'W4766A'          TO POSTSUM-FDNAMN                             
020300     MOVE 'W4766AD2'        TO POSTSUM-DDNAMN2                            
020400     CALL POSTSUM USING POSTSUM-PARM                                      
020500     .                                                                    
020600     EJECT                                                                
020601                                                                          
020610******  IMS-LÄSNINGAR  *******                                            
020620                                                                          
021725 IMS-GU-WDK711 SECTION.                                                   
021726     MOVE 'IMS-GU-WDK711'       TO WS-SEKTION                             
021730                                                                          
021731     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
021732          DELIMITED BY SIZE INTO SSA1                                     
021740     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
021750          DELIMITED BY SIZE INTO SSA2                                     
021760     MOVE '  GE' TO GODK-STATUSKODER                                      
021770     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
021780     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
021790     PERFORM IMS-STATUSKONTROLL                                           
021791     .                                                                    
021792     SKIP3                                                                
021800 IMS-STATUSKONTROLL SECTION.                                              
021900                                                                          
022000     SET STATUS-IX TO 1                                                   
022100     SEARCH GODK-STATUS                                                   
022200       AT END                                                             
022300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022400         DELIMITED BY SIZE INTO FELTEXT                                   
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
023000     SKIP2                                                                
