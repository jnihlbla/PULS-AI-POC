000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5602000.                                                
000400 AUTHOR.         KARL JOHAN HANSSON                                       
000500 DATE-WRITTEN.   96/11/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*    SKAPAR TVÅ KURSPOSTER AV TYP A20,                                    
001100*               EN TILL USA OCH EN TILL CANADA                            
001200*                                                                         
001300*        PROGRAMMET LÄSER KURSBASEN  WDG2                                 
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200                                                                          
002300*          --- UTFIL                                                      
002400     SELECT W56020                     ASSIGN TO W56020D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W56020                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  POST -COPY W510A20 -PRE  UA20-  -L.                                  
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W5602000'.            
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
004300 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
004400                                                                          
004500 01  W-PRKURS-USD            PIC S9(5)V9(5) VALUE +0     COMP-3.          
004600 01  W-PRKURS-CAD            PIC S9(5)V9(5) VALUE +0     COMP-3.          
004700 01  W-PRKURS-UC             PIC S9(5)V9(5) VALUE +0     COMP-3.          
004800 01  W-PRKURS-CU             PIC S9(5)V9(5) VALUE +0     COMP-3.          
004900                                                                          
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
005900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006100     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
006200                                                                          
006300*---- PARAMETRAR TILL ABEND                                               
006400 01  RETURKODER.                                                          
006500     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
006600     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
006700     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
006800                                                                          
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200                                                                          
007300 01  FILLER                      PIC X(16)   VALUE 'A20-AREA'.            
007400                                                                          
007500*01  AREA -COPY W510A20  -PRE A20-                                        
007600                                                                          
007700     EJECT                                                                
007800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007900                                                                          
008000*01  -COPY WDATKORT                                                       
008100     EJECT                                                                
008200                                                                          
008300*01  -COPY W510CURR                                                       
008400     EJECT                                                                
008500 LINKAGE SECTION.                                                         
008600                                                                          
008700*01  -COPY W0008  -PRE WDG2-                                              
008800     05  FILLER                  PIC X.                                   
008900     EJECT                                                                
009000 PROCEDURE DIVISION  USING WDG2-PCB.                                      
009100 MAIN SECTION.                                                            
009200     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
009300                                                                          
009400     OPEN OUTPUT W56020                                                   
009500                                                                          
009600     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
009700     MOVE D-AAR                    TO W-DATE-AAMM(1:2)                    
009800     MOVE D-MAANAD                 TO W-DATE-AAMM(3:2)                    
009900                                                                          
009910     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
009920     MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                   
009940     MOVE 'M'                      TO CURR-KDVALTYP                       
009950                                                                          
010000     MOVE IDPGM                    TO POSTSUM-PROGNAMN                    
010100                                                                          
010200     PERFORM C-BEHANDLA                                                   
010300                                                                          
010400     PERFORM Z-FINIT                                                      
010500                                                                          
010600     MOVE ZERO TO RETURN-CODE                                             
010700     GOBACK                                                               
010800     .                                                                    
010900     EJECT                                                                
011000 C-BEHANDLA SECTION.                                                      
011100                                                                          
011200     MOVE ZERO                TO A20-PRKURS-SU                            
011300     MOVE ZERO                TO A20-PRKURS-SC                            
011400     MOVE ZERO                TO A20-PRKURS-UC                            
011500     MOVE ZERO                TO A20-PRKURS-CU                            
011600     MOVE 'A20'               TO A20-IDPTYP                               
011700     MOVE SPACE               TO A20-KDEKOHT                              
011800     MOVE SPACE               TO A20-IDDC-SEND                            
011900     MOVE SPACE               TO A20-IDDC-REC                             
012000                                                                          
012100     MOVE 'USD'               TO CURR-KDVALISO-ROW                        
012300     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
012400     IF CURR-KDSVAR = ' '                                                 
012500        MOVE CURR-PRKURS-NEW  TO W-PRKURS-USD                             
012600     END-IF                                                               
012700                                                                          
012800     MOVE 'CAD'               TO CURR-KDVALISO-ROW                        
012900     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
013000     IF CURR-KDSVAR = ' '                                                 
013100        MOVE CURR-PRKURS-NEW  TO W-PRKURS-CAD                             
013200     END-IF                                                               
013300                                                                          
013400     IF W-PRKURS-USD > +0 AND W-PRKURS-CAD > +0                           
013500       MOVE CURR-TISTADAT     TO A20-DASTADAT(2:7)                        
013600       MOVE 20                TO A20-DASTADAT(1:2)                        
013700                                                                          
013800       MOVE W-PRKURS-USD      TO A20-PRKURS-SU                            
013900       MOVE W-PRKURS-CAD      TO A20-PRKURS-SC                            
014000       COMPUTE W-PRKURS-UC ROUNDED = W-PRKURS-CAD / W-PRKURS-USD          
014100       MOVE W-PRKURS-UC       TO A20-PRKURS-UC                            
014200       COMPUTE W-PRKURS-CU ROUNDED = W-PRKURS-USD / W-PRKURS-CAD          
014300       MOVE W-PRKURS-CU       TO A20-PRKURS-CU                            
014400                                                                          
014500       MOVE 53                TO A20-IDFTG                                
014600       PERFORM S11-SKRIV-W56020                                           
014700                                                                          
014800       MOVE 54                TO A20-IDFTG                                
014900       PERFORM S11-SKRIV-W56020                                           
015000     END-IF                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 Z-FINIT SECTION.                                                         
015400                                                                          
015500     CLOSE W56020                                                         
015600                                                                          
015700     MOVE 'S' TO POSTSUM-OPKOD                                            
015800     CALL POSTSUM USING POSTSUM-PARM                                      
015900     .                                                                    
016000     EJECT                                                                
016100 S11-SKRIV-W56020 SECTION.                                                
016200                                                                          
016300     WRITE UA20-POST FROM A20-AREA                                        
016400                                                                          
016500     MOVE A20-IDPTYP TO POSTSUM-TRANSTYP                                  
016600     MOVE 'W56020 ' TO POSTSUM-FDNAMN                                     
016700     MOVE 'W56020D2' TO POSTSUM-DDNAMN2                                   
016800     CALL POSTSUM USING POSTSUM-PARM                                      
016900     .                                                                    
017000     EJECT                                                                
