001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4062500.                                                
001600 AUTHOR.         ÖSTRÖM ELEONOR.                                          
001700 DATE-WRITTEN.   03/10/03.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        SHOW INVOICE PER SHIPPING OR SHIPPING PER INVOICE                
002200*                                                                         
002310*        PROGRAMMET LÄSER      WDL5                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T625                                              
002700*        MID:         W4I62501                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O62501                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4062500'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004601*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004602 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004620 77  MAX-RAD                     PIC S9(4)  VALUE +14   COMP SYNC.        
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005510                                                                          
005520 77  W-IDSHIPM-IFYLLD            PIC X       VALUE 'N'.                   
005530     88  IDSHIPM-IFYLLD                      VALUE 'J'.                   
005540     88  IDSHIPM-EJ-IFYLLD                   VALUE 'N'.                   
005550                                                                          
005560 77  W-IDFAKT-IFYLLD             PIC X        VALUE 'N'.                  
005570     88  IDFAKT-IFYLLD                        VALUE 'J'.                  
005580     88  IDFAKT-EJ-IFYLLD                     VALUE 'N'.                  
005590                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '4625'.                
005800     88  GODK-MID                            VALUE '4621' '4622'          
005900                                                   '4623' '4624'          
006000                                                   '4625' '4626'          
006100                                                   '4627' '4628'          
006200                                                   '4629'.                
006300     88  HELP-MID                            VALUE '0551'.                
006310 77  WX-IDTRANS                  PIC X(4)    VALUE SPACE.                 
006400     EJECT                                                                
006412*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
006420 01  TABENTRY-PARM.                                                       
006430     03  STEGLANGD               PIC S9(9) COMP  VALUE 22.                
006440     03  ANTAL                   PIC S9(9) COMP.                          
006450     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 7.                 
006460                                                                          
006470 01  IX-RAD                      PIC S9(3) COMP-3 VALUE 1.                
006480 01  IX-RAD-TAB                  PIC S9(3) COMP-3 VALUE 1.                
006481 01  W-TEST-IDSHIPM              PIC 9(7) VALUE ZERO.                     
006483 01  W-TESTTAB-IDSHIPM           PIC 9(7) VALUE ZERO.                     
006490                                                                          
006491 01  TAB-MAX                     PIC S9(9) COMP VALUE 1000.               
006492     EJECT                                                                
006493*    --- TABELL SOM SORTERAS AV WINTSOR                                   
006494 01  TABELL.                                                              
006495     03  TAB-POST  OCCURS 1000.                                           
006496       04  TAB-RAD.                                                       
006497         05  TAB-IDSHIPM         PIC 9(7).                                
006498         05  TAB-TISKEPPN        PIC S9(7) COMP-3.                        
006499         05  TAB-IDFAKT          PIC S9(7) COMP-3.                        
006500         05  TAB-TIFAKT          PIC S9(7) COMP-3.                        
006501         05  TAB-IDDISTR         PIC S9(5) COMP-3.                        
006510                                                                          
006520*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007101     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007801     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007810     03  INF-MORE-INFO-EXIST     PIC X(3)    VALUE '105'.                 
007820     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
008001     EJECT                                                                
008002 01  W-EJ-TILLATEN-TEXT          PIC X(55)   VALUE                        
008003       'SHIPM NO AND INVOICE NR NOT ALLOWED AT THE SAME TIME   '.         
008004     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008900*                                                                         
009000 01  WS-MSGI-SPAR-AREA.                                                   
009100     03  SPAR-IDTRANS           PIC X(4)    VALUE '4625'.                 
009200     03  WS-MSGI-RADNR          PIC 9(3)    VALUE  1.                     
009201     03  WS-MSGI-RADNR-ENTER    PIC 9(3)    VALUE  1.                     
009202     03  WS-MSGI-RADNR-NEXT     PIC 9(3)    VALUE  1.                     
009203     03  WS-MSGI-PGM            PIC X(6)    VALUE  SPACE.                 
009204     03  SPAR-IDSHIPM-IFYLLD    PIC X       VALUE  SPACE.                 
009205     03  SPAR-IDFAKT-IFYLLD     PIC X       VALUE  SPACE.                 
009206     03  SPAR-IDSHIPM-NEXT      PIC 9(7).                                 
009208     03  SPAR-IDFAKT-NEXT       PIC S9(7) COMP-3.                         
009209     03  SPAR-IDPRODNR-NEXT     PIC S9(7) COMP-3.                         
009210     03  SPAR-IDKOLLI-NEXT      PIC S9(5) COMP-3.                         
009211     03  SPAR-IDARTNR-NEXT      PIC S9(9) COMP-3.                         
009220     03  SPAR-IDSHIPM-ENTER     PIC 9(7)  VALUE ZERO.                     
009240     03  SPAR-IDFAKT-ENTER      PIC S9(7) COMP-3.                         
009250     03  SPAR-IDPRODNR-ENTER    PIC S9(7) COMP-3.                         
009260     03  SPAR-IDKOLLI-ENTER     PIC S9(5) COMP-3.                         
009270     03  SPAR-IDARTNR-ENTER     PIC S9(9) COMP-3.                         
009300     EJECT                                                                
009400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W4I62501                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400     03  MOD REDEFINES MSG-AREA.                                          
010500*      05  -COPY W4O62501                                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010800     SKIP3                                                                
010900*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011601*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011602 01  NYCKLAR-TILL-DLI.                                                    
011603     03  W-IDFAKT-X.                                                      
011604         05  W-IDFAKT            PIC S9(7) COMP-3.                        
011609                                                                          
011646     03  W-WDL5A1KY-MIN-X.                                                
011647         05  W-SEQA-IDSHIPM-MIN  PIC 9(7).                                
011648         05  W-SEQA-IDFAKT-MIN   PIC S9(7) VALUE +0 COMP-3.               
011649         05  FILLER              PIC X(7)  VALUE LOW-VALUE.               
011680                                                                          
011690     03  W-WDL5A1KY-MAX-X.                                                
011691         05  W-SEQA-IDSHIPM-MAX  PIC 9(7).                                
011692         05  FILLER              PIC X(11) VALUE HIGH-VALUE.              
011698                                                                          
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012210     88  BASEN-SLUT                          VALUE 'GB'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(256).                              
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL501'.                      
013602 01  DLI-IO-WDL501.                                                       
013610*    03  -COPY WDL501                                                     
013900     EJECT                                                                
013901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL511'.                      
013902 01  DLI-IO-WDL511.                                                       
013903*    03  -COPY WDL511                                                     
013904     EJECT                                                                
013910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL5A1'.                      
013920 01  DLI-IO-WDL5A1.                                                       
013930*    03  -COPY WDL5A1                                                     
013940     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014200*01  -COPY W0008   -PRE WDP7-                                             
014300     05  FILLER                  PIC X.                                   
014401                                                                          
014402*01  -COPY W0008  -PRE WDL5-                                              
014410     05  FILLER                  PIC X.                                   
014411                                                                          
014420*01  -COPY W0008  -PRE WDL5A-                                             
014430     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014601 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDL5-PCB WDL5A-PCB.           
014602 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDL5-PCB WDL5A-PCB.           
014700                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FINNS                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-KOLLA-NYCKLAR                                            
015300       IF NYCKLAR-OK                                                      
015400         IF MFS-FIRST                                                     
015500           PERFORM C-FOERSTA-SIDA                                         
015600         ELSE                                                             
015700           IF MFS-NEXT                                                    
015710             PERFORM D-NAESTA-SIDA                                        
015720           ELSE                                                           
015730             PERFORM E-SAMMA-SIDA                                         
015740           END-IF                                                         
015750         END-IF                                                           
015800         PERFORM F-LAES-VISA-INFO                                         
015900       END-IF                                                             
016000*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
016100*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O62501 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I62501                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I62501                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018510                         WX-IDTRANS                                       
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W4O625N1' TO MFS-IDMOD                                         
018900     MOVE '4625' TO MOD-IDTRANS                                           
019000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF EGEN-MID OR HELP-MID                                              
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-NYCKLAR SECTION.                                                 
020300                                                                          
020400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020500     MOVE '001'             TO MSGI-KDCALL                                
020600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020800     MOVE '4625'            TO MSGI-IDTRANS                               
020810                                                                          
020820     MOVE JA                    TO NYCKLAR-SW                             
020850                                                                          
020901     IF GODK-MID                                                          
020902       MOVE MID-IDSHIPM-IN       TO MSGI-IDSHIPM                          
020903       MOVE MID-IDFAKT-IN        TO MSGI-IDFAKT                           
020910       IF MID-IDSHIPM-IN NUMERIC AND MID-IDSHIPM-IN > ZERO                
020920         AND MID-IDFAKT-IN = ALL '+'                                      
021000         MOVE SPACE              TO MSGI-IDFAKT                           
021003       END-IF                                                             
021004       IF MID-IDFAKT-IN NUMERIC AND MID-IDFAKT-IN > ZERO                  
021005         AND MID-IDSHIPM-IN = ALL '+'                                     
021006         MOVE SPACE              TO MSGI-IDSHIPM                          
021008       END-IF                                                             
021009       IF MID-IDSHIPM-IN = ALL ZERO OR                                    
021010         MID-IDFAKT-IN = ALL ZERO                                         
021020           MOVE SPACE            TO MSGI-IDSHIPM                          
021030                                    MSGI-IDFAKT                           
021040       END-IF                                                             
021100     END-IF                                                               
021200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021300     MOVE MSGI-SPAR-AREA TO WS-MSGI-SPAR-AREA                             
021400     IF WX-IDTRANS NOT = '4625'                                           
021500       PERFORM S11-NOLLA-SAVE                                             
021501     ELSE                                                                 
021502       IF SPAR-IDFAKT-ENTER NOT NUMERIC                                   
021503         MOVE ZERO TO SPAR-IDFAKT-ENTER                                   
021504       END-IF                                                             
021505       IF SPAR-IDPRODNR-ENTER NOT NUMERIC                                 
021506         MOVE ZERO TO SPAR-IDPRODNR-ENTER                                 
021507       END-IF                                                             
021508       IF SPAR-IDKOLLI-ENTER NOT NUMERIC                                  
021509         MOVE ZERO TO SPAR-IDKOLLI-ENTER                                  
021510       END-IF                                                             
021517       IF SPAR-IDFAKT-NEXT NOT NUMERIC                                    
021518         MOVE ZERO TO SPAR-IDFAKT-NEXT                                    
021519       END-IF                                                             
021520       IF SPAR-IDPRODNR-NEXT NOT NUMERIC                                  
021521         MOVE ZERO TO SPAR-IDPRODNR-NEXT                                  
021522       END-IF                                                             
021523       IF SPAR-IDKOLLI-NEXT NOT NUMERIC                                   
021524         MOVE ZERO TO SPAR-IDKOLLI-NEXT                                   
021525       END-IF                                                             
021532     END-IF                                                               
021533                                                                          
021534*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
021600     MOVE 'GB'             TO MED-IDSKYLT                                 
021700                                                                          
022025*    -- KONTROLL AV NYCKLAR                                               
022042     MOVE MFS-RENSA-FAELT  TO MOD-IDFAKT-IN                               
022043                              MOD-IDSHIPM-IN                              
022044                                                                          
022102     IF MID-IDSHIPM-IN NOT = ALL '+'                                      
022103       MOVE '7'         TO MFS-IDPFK                                      
022104       MOVE SPACE       TO MFS-KDTRTYP                                    
022105     ELSE                                                                 
022106       IF MID-IDFAKT-IN NOT = ALL '+'                                     
022107         MOVE '7'         TO MFS-IDPFK                                    
022108         MOVE SPACE       TO MFS-KDTRTYP                                  
022109       END-IF                                                             
022110     END-IF                                                               
022111                                                                          
022112     IF MFS-FIRST                                                         
022113       IF MSGI-IDSHIPM > SPACE AND MSGI-IDSHIPM NUMERIC                   
022114         AND MSGI-IDSHIPM > ZERO                                          
022116           MOVE JA  TO SPAR-IDSHIPM-IFYLLD                                
022118                       W-IDSHIPM-IFYLLD                                   
022119           INSPECT MSGI-IDSHIPM REPLACING LEADING SPACE BY ZERO           
022120           IF MSGI-IDSHIPM NUMERIC AND MSGI-IDSHIPM > ZERO                
022121             MOVE MSGI-IDSHIPM TO W-SEQA-IDSHIPM-MIN                      
022122                                  W-SEQA-IDSHIPM-MAX                      
022123           END-IF                                                         
022129       ELSE                                                               
022130         IF MSGI-IDFAKT > SPACE AND MSGI-IDFAKT NUMERIC                   
022131           AND MSGI-IDFAKT > ZERO                                         
022132           MOVE JA                  TO SPAR-IDFAKT-IFYLLD                 
022133                                       W-IDFAKT-IFYLLD                    
022134           INSPECT MSGI-IDFAKT REPLACING LEADING SPACE BY ZERO            
022135           IF MSGI-IDFAKT NUMERIC AND MSGI-IDFAKT > ZERO                  
022136             MOVE MSGI-IDFAKT  TO W-IDFAKT                                
022138           END-IF                                                         
022139         ELSE                                                             
022155           MOVE NEJ TO NYCKLAR-SW                                         
022156           MOVE NEJ            TO W-IDSHIPM-IFYLLD                        
022157                                  W-IDFAKT-IFYLLD                         
022158                                  SPAR-IDSHIPM-IFYLLD                     
022159                                  SPAR-IDFAKT-IFYLLD                      
022160           MOVE SPACE          TO MSGI-IDSHIPM                            
022161                                  MSGI-IDFAKT                             
022162         END-IF                                                           
022163       END-IF                                                             
022164     END-IF                                                               
022165                                                                          
022166     IF MSGI-IDSHIPM > SPACE AND MSGI-IDFAKT > SPACE                      
022167       MOVE NEJ TO NYCKLAR-SW                                             
022171       MOVE W-EJ-TILLATEN-TEXT TO MOD-TEMFSINF                            
022172       MOVE JA                 TO W-IDSHIPM-IFYLLD                        
022173                                  W-IDFAKT-IFYLLD                         
022174                                  SPAR-IDSHIPM-IFYLLD                     
022175                                  SPAR-IDFAKT-IFYLLD                      
022176     END-IF                                                               
022199                                                                          
022200     IF MFS-NEXT OR MFS-ENTER                                             
022210       IF MSGI-IDSHIPM > SPACE AND MSGI-IDSHIPM NUMERIC                   
022211         AND MSGI-IDSHIPM > ZERO                                          
022220           MOVE JA  TO SPAR-IDSHIPM-IFYLLD                                
022230                       W-IDSHIPM-IFYLLD                                   
022231           INSPECT MSGI-IDSHIPM REPLACING LEADING SPACE BY ZERO           
022232           INSPECT MSGI-IDFAKT  REPLACING LEADING SPACE BY ZERO           
022236       ELSE                                                               
022237         IF MSGI-IDFAKT > SPACE AND MSGI-IDFAKT NUMERIC                   
022238           AND MSGI-IDFAKT > ZERO                                         
022239           MOVE JA                  TO SPAR-IDFAKT-IFYLLD                 
022240                                       W-IDFAKT-IFYLLD                    
022241           INSPECT MSGI-IDFAKT REPLACING LEADING SPACE BY ZERO            
022242           IF MSGI-IDFAKT NUMERIC AND MSGI-IDFAKT > ZERO                  
022243             MOVE MSGI-IDFAKT  TO W-IDFAKT                                
022245           END-IF                                                         
022246         ELSE                                                             
022247           MOVE NEJ TO NYCKLAR-SW                                         
022248           MOVE NEJ            TO W-IDSHIPM-IFYLLD                        
022249                                  W-IDFAKT-IFYLLD                         
022250                                  SPAR-IDSHIPM-IFYLLD                     
022251                                  SPAR-IDFAKT-IFYLLD                      
022252         END-IF                                                           
022253       END-IF                                                             
022254     END-IF                                                               
022255                                                                          
022256     IF IDSHIPM-IFYLLD AND IDFAKT-IFYLLD                                  
022257        MOVE NEJ TO NYCKLAR-SW                                            
022258        MOVE W-EJ-TILLATEN-TEXT    TO MOD-TEMFSINF                        
022259     ELSE                                                                 
022260        IF IDSHIPM-EJ-IFYLLD AND IDFAKT-EJ-IFYLLD                         
022261           MOVE NEJ              TO NYCKLAR-SW                            
022262        END-IF                                                            
022263     END-IF                                                               
022264                                                                          
022265     IF GODK-MID                                                          
022266       IF NYCKLAR-OK                                                      
022267       IF IDSHIPM-IFYLLD                                                  
022268         MOVE MSGI-IDSHIPM         TO MOD-IDSHIPM-UT                      
022269         INSPECT MOD-IDSHIPM-UT REPLACING LEADING ZERO BY SPACE           
022270         MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-UT                            
022271         MOVE NEJ                  TO SPAR-IDFAKT-IFYLLD                  
022272       END-IF                                                             
022273       IF IDFAKT-IFYLLD                                                   
022274         MOVE MSGI-IDFAKT          TO MOD-IDFAKT-UT                       
022275         INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE            
022276         MOVE MFS-RENSA-FAELT TO MOD-IDSHIPM-UT                           
022277         MOVE NEJ                  TO SPAR-IDSHIPM-IFYLLD                 
022278       END-IF                                                             
022279       END-IF                                                             
022280     ELSE                                                                 
022281       MOVE MFS-RENSA-FAELT TO MOD-IDSHIPM-UT                             
022282       MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-UT                              
022283     END-IF                                                               
022284                                                                          
022290                                                                          
022300     IF NYCKLAR-FEL                                                       
022410       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022500       CALL WMEDKONV USING MED-WMEDAREA                                   
022600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022700       PERFORM MFS-RENSA-FAELT-IN                                         
022800       PERFORM MFS-RENSA-FAELT-UT                                         
022810       MOVE MFS-RENSA-FAELT TO MOD-IDSHIPM-UT                             
022820                               MOD-IDFAKT-UT                              
022900     END-IF                                                               
022904                                                                          
022910     MOVE '002'      TO MSGI-KDCALL                                       
022920     MOVE '4625'   TO SPAR-IDTRANS                                        
022930     MOVE WS-MSGI-SPAR-AREA  TO MSGI-SPAR-AREA                            
022931     IF MSGI-IDSHIPM NOT NUMERIC                                          
022932       MOVE SPACE             TO MSGI-IDSHIPM                             
022933     END-IF                                                               
022940     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023000     .                                                                    
023101     EJECT                                                                
023102 C-FOERSTA-SIDA SECTION.                                                  
023103                                                                          
023104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023105     CALL WMEDKONV USING MED-WMEDAREA                                     
023106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
023107     MOVE +1                    TO IX-RAD-TAB                             
023109     MOVE ZERO                  TO W-TESTTAB-IDSHIPM                      
023110                                                                          
023111     PERFORM MFS-RENSA-FAELT-IN                                           
023112     .                                                                    
023113     EJECT                                                                
023114 D-NAESTA-SIDA SECTION.                                                   
023115                                                                          
023120     IF SPAR-IDTRANS = '4625'                                             
023130       IF IDSHIPM-IFYLLD                                                  
023140         MOVE SPAR-IDSHIPM-NEXT  TO W-SEQA-IDSHIPM-MIN                    
023141         MOVE SPAR-IDSHIPM-NEXT  TO W-SEQA-IDSHIPM-MAX                    
023142         MOVE SPAR-IDFAKT-NEXT   TO W-SEQA-IDFAKT-MIN                     
023147       ELSE                                                               
023148         IF IDFAKT-IFYLLD                                                 
023149           MOVE MSGI-IDFAKT      TO W-IDFAKT                              
023151         END-IF                                                           
023160       END-IF                                                             
023190     ELSE                                                                 
023200       PERFORM MFS-RENSA-FAELT-IN                                         
023300     END-IF                                                               
023310     .                                                                    
023320     EJECT                                                                
023330 E-SAMMA-SIDA SECTION.                                                    
023340     IF SPAR-IDTRANS = '4625' OR '0551'                                   
023350       IF IDSHIPM-IFYLLD                                                  
023360         MOVE SPAR-IDSHIPM-ENTER  TO W-SEQA-IDSHIPM-MIN                   
023361         MOVE SPAR-IDSHIPM-ENTER  TO W-SEQA-IDSHIPM-MAX                   
023362         MOVE SPAR-IDFAKT-ENTER   TO W-SEQA-IDFAKT-MIN                    
023367       ELSE                                                               
023368         IF IDFAKT-IFYLLD                                                 
023369           MOVE MSGI-IDFAKT      TO W-IDFAKT                              
023371         END-IF                                                           
023380       END-IF                                                             
023403     ELSE                                                                 
023404       PERFORM MFS-RENSA-FAELT-IN                                         
023405     END-IF                                                               
023406     .                                                                    
023407     EJECT                                                                
023410 F-LAES-VISA-INFO SECTION.                                                
023500                                                                          
023510     IF IDSHIPM-IFYLLD                                                    
023520        PERFORM FA-LAES-FAKTURA                                           
023530     END-IF                                                               
023540     IF IDFAKT-IFYLLD                                                     
023600        PERFORM FB-LAES-SKEPPNING                                         
023610        PERFORM FC-SORTERA-SKEPPNING                                      
023620        PERFORM FD-VISA-SKEPPNING                                         
023700     END-IF                                                               
023710     .                                                                    
023720     EJECT                                                                
023730                                                                          
023740 FA-LAES-FAKTURA SECTION.                                                 
023750                                                                          
023760     PERFORM IMS-GU-WDL5A1                                                
023800     IF SEGMENT-SAKNAS                                                    
023900        MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                              
024000        CALL WMEDKONV USING MED-WMEDAREA                                  
024100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024200        PERFORM MFS-RENSA-FAELT-UT                                        
024300     ELSE                                                                 
024403       MOVE SEQA-IDSHIPM    TO SPAR-IDSHIPM-ENTER                         
024404       MOVE SEQA-IDFAKT     TO SPAR-IDFAKT-ENTER                          
024406       MOVE SEQA-IDPRODNR   TO SPAR-IDPRODNR-ENTER                        
024407       MOVE SEQA-IDKOLLI    TO SPAR-IDKOLLI-ENTER                         
024410                                                                          
024412       MOVE +1              TO IX-RAD                                     
024413       PERFORM UNTIL IX-RAD > MAX-RAD                                     
024415         IF SEGMENT-FINNS                                                 
024417           MOVE SEQA-IDFAKT     TO MOD-IDFAKT   (IX-RAD)                  
024418           MOVE SEQA-TIFAKT     TO MOD-TIFAKT   (IX-RAD)                  
024419           MOVE SEQA-IDDISTR    TO MOD-IDDISTR  (IX-RAD)                  
024422           MOVE SEQA-IDSHIPM    TO MOD-IDSHIPM  (IX-RAD)                  
024423           MOVE SEQA-TISKEPPN   TO MOD-TISKEPPN (IX-RAD)                  
024426           ADD 1 TO IX-RAD                                                
024428           COMPUTE W-SEQA-IDFAKT-MIN = SEQA-IDFAKT + 1                    
024429           PERFORM IMS-GN-WDL5A1                                          
024430         ELSE                                                             
024431           MOVE MFS-RENSA-FAELT TO MOD-IDFAKT   (IX-RAD)                  
024432           MOVE MFS-RENSA-FAELT TO MOD-TIFAKT   (IX-RAD)                  
024433           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR  (IX-RAD)                  
024434           MOVE MFS-RENSA-FAELT TO MOD-IDSHIPM  (IX-RAD)                  
024435           MOVE MFS-RENSA-FAELT TO MOD-TISKEPPN (IX-RAD)                  
024436           ADD 1 TO IX-RAD                                                
024437         END-IF                                                           
024439       END-PERFORM                                                        
024449                                                                          
024450       IF SEGMENT-FINNS                                                   
024451         MOVE SEQA-IDSHIPM  TO SPAR-IDSHIPM-NEXT                          
024452         MOVE SEQA-IDFAKT   TO SPAR-IDFAKT-NEXT                           
024453         MOVE SEQA-IDPRODNR TO SPAR-IDPRODNR-NEXT                         
024454         MOVE SEQA-IDKOLLI  TO SPAR-IDKOLLI-NEXT                          
024457         MOVE INF-MORE-INFO-EXIST TO MED-IDMFSINF                         
024458         CALL WMEDKONV         USING MED-WMEDAREA                         
024459         MOVE MED-MFSINF          TO MOD-TEMFSINF                         
024460       ELSE                                                               
024461         MOVE SPAR-IDSHIPM-ENTER  TO SPAR-IDSHIPM-NEXT                    
024462         MOVE SPAR-IDFAKT-ENTER   TO SPAR-IDFAKT-NEXT                     
024463         MOVE SPAR-IDPRODNR-ENTER TO SPAR-IDPRODNR-NEXT                   
024464         MOVE SPAR-IDKOLLI-ENTER  TO SPAR-IDKOLLI-NEXT                    
024467         MOVE INF-LAST-PAGE       TO MED-IDMFSINF                         
024468         CALL WMEDKONV         USING MED-WMEDAREA                         
024469         MOVE MED-TEMFSINF        TO MOD-TEMFSINF                         
024470       END-IF                                                             
024471                                                                          
024472       MOVE '002'      TO MSGI-KDCALL                                     
024473       MOVE '4625'   TO SPAR-IDTRANS                                      
024474       MOVE WS-MSGI-SPAR-AREA  TO MSGI-SPAR-AREA                          
024480       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024710 FB-LAES-SKEPPNING SECTION.                                               
024720                                                                          
024730     PERFORM IMS-GU-WDL501                                                
024740                                                                          
024760     IF SEGMENT-SAKNAS                                                    
024770       MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                               
024780       CALL WMEDKONV USING MED-WMEDAREA                                   
024790       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024791       PERFORM MFS-RENSA-FAELT-UT                                         
024793     ELSE                                                                 
024794       PERFORM IMS-GNP-WDL511                                             
024802       MOVE +1  TO IX-RAD                                                 
024811       PERFORM UNTIL SEGMENT-SAKNAS OR IX-RAD > TAB-MAX                   
024814         IF FAKC-IDSHIPM NOT = W-TEST-IDSHIPM                             
024815            MOVE FAKC-IDSHIPM  TO TAB-IDSHIPM  (IX-RAD)                   
024816            MOVE FAKC-TISKEPPN TO TAB-TISKEPPN (IX-RAD)                   
024817            MOVE FAK-IDFAKT    TO TAB-IDFAKT   (IX-RAD)                   
024818            MOVE FAKC-TIFAKT   TO TAB-TIFAKT   (IX-RAD)                   
024819            MOVE FAKC-IDDISTR  TO TAB-IDDISTR  (IX-RAD)                   
024820            MOVE FAKC-IDSHIPM  TO W-TEST-IDSHIPM                          
024821            ADD 1 TO IX-RAD                                               
024822         END-IF                                                           
024827         PERFORM IMS-GNP-WDL511                                           
024828       END-PERFORM                                                        
024829                                                                          
024845     END-IF                                                               
024846                                                                          
024847     MOVE '002'      TO MSGI-KDCALL                                       
024848     MOVE '4625'   TO SPAR-IDTRANS                                        
024851     MOVE WS-MSGI-SPAR-AREA  TO MSGI-SPAR-AREA                            
024852     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
024853     .                                                                    
024854     EJECT                                                                
024860 FC-SORTERA-SKEPPNING SECTION.                                            
024870                                                                          
024880     SUBTRACT 1 FROM IX-RAD                                               
024890     MOVE IX-RAD TO ANTAL                                                 
024900                                                                          
025000     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
025100                  TAB-IDSHIPM (1) NYCKELLANGD                             
025200     .                                                                    
025300     EJECT                                                                
025400 FD-VISA-SKEPPNING SECTION.                                               
025500                                                                          
025710     IF MFS-IDPFK = '8' AND WS-MSGI-PGM = 'W40625'                        
025720        MOVE WS-MSGI-RADNR-NEXT        TO IX-RAD-TAB                      
025721        MOVE ZERO                      TO W-TESTTAB-IDSHIPM               
025730     ELSE                                                                 
025740        IF MFS-IDPFK = '7'                                                
025750           MOVE +1                     TO IX-RAD-TAB                      
025751                                          WS-MSGI-RADNR                   
025752           MOVE ZERO                   TO W-TESTTAB-IDSHIPM               
025760        ELSE                                                              
025770           IF MFS-IDPFK = ' '                                             
025780              MOVE WS-MSGI-RADNR-ENTER TO IX-RAD-TAB                      
025781              MOVE ZERO                TO W-TESTTAB-IDSHIPM               
025790           END-IF                                                         
025791        END-IF                                                            
025792     END-IF                                                               
025793                                                                          
025794     IF WS-MSGI-RADNR = 1                                                 
025795       MOVE ZERO                         TO W-TESTTAB-IDSHIPM             
025798     END-IF                                                               
025799                                                                          
025800     MOVE +1                           TO IX-RAD                          
025801     IF IX-RAD-TAB < +1                                                   
025802       MOVE 1                          TO IX-RAD-TAB                      
025803     END-IF                                                               
025804                                                                          
025805     PERFORM UNTIL IX-RAD > MAX-RAD                                       
025806       OR IX-RAD-TAB > TAB-MAX                                            
025807       OR TAB-IDSHIPM (IX-RAD-TAB) = SPACE                                
025808       OR TAB-IDSHIPM (IX-RAD-TAB) = LOW-VALUE                            
025809         IF TAB-IDSHIPM (IX-RAD-TAB) NOT = W-TESTTAB-IDSHIPM              
025810           MOVE TAB-IDSHIPM (IX-RAD-TAB) TO MOD-IDSHIPM (IX-RAD)          
025811           MOVE TAB-TISKEPPN (IX-RAD-TAB) TO MOD-TISKEPPN (IX-RAD)        
025812           MOVE TAB-IDFAKT   (IX-RAD-TAB) TO MOD-IDFAKT   (IX-RAD)        
025813           MOVE TAB-TIFAKT   (IX-RAD-TAB) TO MOD-TIFAKT   (IX-RAD)        
025814           MOVE TAB-IDDISTR  (IX-RAD-TAB) TO MOD-IDDISTR  (IX-RAD)        
025815           MOVE TAB-IDSHIPM (IX-RAD-TAB) TO W-TESTTAB-IDSHIPM             
025816           IF IX-RAD = 1                                                  
025817              MOVE TAB-IDSHIPM (IX-RAD-TAB) TO SPAR-IDSHIPM-ENTER         
025818              MOVE IX-RAD-TAB               TO WS-MSGI-RADNR-ENTER        
025819           END-IF                                                         
025820           ADD +1             TO IX-RAD                                   
025821         END-IF                                                           
025822         ADD +1               TO IX-RAD-TAB                               
025823     END-PERFORM                                                          
025824                                                                          
025826     IF IX-RAD-TAB      <= ANTAL                                          
025827        MOVE W-TESTTAB-IDSHIPM    TO SPAR-IDSHIPM-NEXT                    
025828        MOVE IX-RAD-TAB           TO WS-MSGI-RADNR-NEXT                   
025829        MOVE 'W40625'             TO WS-MSGI-PGM                          
025830        IF ANTAL > ZERO                                                   
025831          MOVE INF-MORE-INFO-EXIST                                        
025832                                TO MED-IDMFSINF                           
025833        END-IF                                                            
025834        CALL WMEDKONV USING MED-WMEDAREA                                  
025835        MOVE MED-MFSINF       TO MOD-TEMFSFEL                             
025836     ELSE                                                                 
025837        MOVE SPAR-IDSHIPM-ENTER   TO SPAR-IDSHIPM-NEXT                    
025838        MOVE WS-MSGI-RADNR-ENTER  TO WS-MSGI-RADNR                        
025839        SUBTRACT 1 FROM IX-RAD-TAB                                        
025840        MOVE IX-RAD-TAB           TO WS-MSGI-RADNR-NEXT                   
025841        MOVE 'W40625'             TO WS-MSGI-PGM                          
025842        IF ANTAL > ZERO                                                   
025843          MOVE INF-LAST-PAGE        TO MED-IDMFSINF                       
025844        END-IF                                                            
025845        CALL WMEDKONV         USING MED-WMEDAREA                          
025846        MOVE MED-TEMFSINF         TO MOD-TEMFSINF                         
025847     END-IF                                                               
025848                                                                          
025849     MOVE '002'               TO MSGI-KDCALL                              
025850     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
025851     MOVE '4625'              TO MSGI-IDTRANS                             
025852     MOVE WS-MSGI-SPAR-AREA   TO MSGI-SPAR-AREA                           
025853     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
025854     .                                                                    
025855     EJECT                                                                
025856 S11-NOLLA-SAVE    SECTION.                                               
025857                                                                          
025859     MOVE ZERO               TO SPAR-IDFAKT-ENTER                         
025862     MOVE ZERO               TO SPAR-IDPRODNR-ENTER                       
025865     MOVE ZERO               TO SPAR-IDKOLLI-ENTER                        
025874     MOVE ZERO               TO SPAR-IDFAKT-NEXT                          
025877     MOVE ZERO               TO SPAR-IDPRODNR-NEXT                        
025880     MOVE ZERO               TO SPAR-IDKOLLI-NEXT                         
025887     MOVE 1                  TO WS-MSGI-RADNR                             
025888                                WS-MSGI-RADNR-ENTER                       
025889                                WS-MSGI-RADNR-NEXT                        
025890     .                                                                    
025891     EJECT                                                                
025892 MFS-RENSA-FAELT-UT SECTION.                                              
025900                                                                          
026000*    --- ALLA UTDATA-FÄLT                                                 
026110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
026113     MOVE +1 TO INDX                                                      
026114     PERFORM UNTIL INDX > MAX-RAD                                         
026120       MOVE MFS-RENSA-FAELT TO MOD-IDSHIPM (INDX)                         
026130                               MOD-TISKEPPN (INDX)                        
026140                               MOD-IDFAKT (INDX)                          
026150                               MOD-TIFAKT (INDX)                          
026160                               MOD-IDDISTR (INDX)                         
026161       ADD +1 TO INDX                                                     
026170     END-PERFORM                                                          
026400     .                                                                    
026501     SKIP3                                                                
026700 MFS-RENSA-FAELT-IN SECTION.                                              
026800                                                                          
026900*    --- ALLA INDATA-FÄLT                                                 
027000     MOVE MFS-RENSA-FAELT TO MOD-IDSHIPM-IN                               
027100                             MOD-IDFAKT-IN                                
027200     .                                                                    
027300     EJECT                                                                
030400* --- IMS SEKTIONER ---                                                   
030500     SKIP3                                                                
030600 IMS-GET-MSG SECTION.                                                     
030700                                                                          
030800     MOVE '  QC' TO GODK-STATUSKODER                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     SKIP3                                                                
031400 IMS-INSERT-MSG SECTION.                                                  
031500                                                                          
031900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032000     MOVE SPACE TO GODK-STATUSKODER                                       
032100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032300     PERFORM IMS-STATUSKONTROLL                                           
032400     .                                                                    
032501     EJECT                                                                
032502 IMS-GU-WDL501 SECTION.                                                   
032503                                                                          
032504     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
032508          DELIMITED BY SIZE INTO SSA1                                     
032509     MOVE '  GE' TO GODK-STATUSKODER                                      
032510     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-WDL501 SSA1                    
032511     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
032512     PERFORM IMS-STATUSKONTROLL                                           
032520     .                                                                    
032600     EJECT                                                                
032610 IMS-GNP-WDL511 SECTION.                                                  
032620                                                                          
032630     MOVE 'WDL511 '  TO SSA1                                              
032650     MOVE '  GE' TO GODK-STATUSKODER                                      
032660     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL511 SSA1                   
032670     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
032680     PERFORM IMS-STATUSKONTROLL                                           
032690     .                                                                    
032691     EJECT                                                                
032692 IMS-GU-WDL5A1 SECTION.                                                   
032693                                                                          
032694     STRING 'WDL5A1  (WDL5A1KY>=' W-WDL5A1KY-MIN-X                        
032695                    '&WDL5A1KY<=' W-WDL5A1KY-MAX-X ')'                    
032696          DELIMITED BY SIZE INTO SSA1                                     
032697     MOVE '  GE' TO GODK-STATUSKODER                                      
032698     CALL CBLTDLI USING GU WDL5A-PCB DLI-IO-WDL5A1 SSA1                   
032699     MOVE WDL5A-STATUS-CODE TO STATUS-WS                                  
032700     PERFORM IMS-STATUSKONTROLL                                           
032701     .                                                                    
032702     EJECT                                                                
032703 IMS-GN-WDL5A1 SECTION.                                                   
032704                                                                          
032705     STRING 'WDL5A1  (WDL5A1KY>=' W-WDL5A1KY-MIN-X                        
032706                    '&WDL5A1KY<=' W-WDL5A1KY-MAX-X ')'                    
032707          DELIMITED BY SIZE INTO SSA1                                     
032708     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032709     CALL CBLTDLI USING GN WDL5A-PCB DLI-IO-WDL5A1 SSA1                   
032710     MOVE WDL5A-STATUS-CODE TO STATUS-WS                                  
032711     PERFORM IMS-STATUSKONTROLL                                           
032712     .                                                                    
032713     EJECT                                                                
032720 IMS-STATUSKONTROLL SECTION.                                              
032800                                                                          
032900     SET STATUS-IX TO 1                                                   
033000     SEARCH GODK-STATUS                                                   
033100       AT END                                                             
033200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033300         DELIMITED BY SIZE INTO FELTEXT                                   
033400         CALL FELLOG                                                      
033500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
