000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1521000.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/04/23.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KONVERTERING AV ÖVERSATT TEXT FRÅN UNICODE/UCS2                  
001100*        TILL IBMS OLIKA EBCDIC-KODER.                                    
001200*                                                                         
001300*        TEXTEN FINNS I EN FIL SOM HÄMTATS FRÅN                           
001400*        ÖVERSÄTTNINGSBYRÅN CBG'S SERVER I FTP-JOBB W152J009.             
001500*        DEN TYP AV TEXT SOM FILEN KAN INNEHÅLLA                          
001600*        ÄR BENÄMNINGAR. (PTYP ='PBEN' )                                  
001700*        FÖRR VAR DET ÄVEN RUBRIKER, FOTNÖTTER OCH TILLÄGGS-              
001800*        TEXTER.                                                          
001900*                                                                         
002000*        INFILEN HAR TRANSMITERATS MED FTP BINARY MODE.                   
002100*        ENSKILDA POSTER MÅSTE DÄRFÖR SEPARERAS GENOM ATT                 
002200*        SCANNA EFTER CR/LF. EN POST KAN SPÄNNA ÖVER FLERA                
002300*        "FYSISKA" LOGISKA RECORDS.                                       
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  OM POSTTYP (TYP AV TEXT) ÄR OGILTIG.                    
002700*              -  OM SPRÅKKODEN ÄR OGILTIG.                               
002800*                                                                         
002900*    ÄNDRING:                                                             
003000*        98-05-29  PBEN-POSTEN ÄNDRAS TILL ATT INTE INNEHÅLLA             
003100*        ARTIKELNUMMER.  "TYP2-"  BORTFALLER DÅ.                          
003200*                                                                         
003300*      2005-09-22  PBEN SIMPLIFIED CHINESE TILLKOMMER (RCN, CN)           
003400*                                                                         
003500*      2007-08-20  ANPASSAS FÖR NYA FILNAMN-SÄTTNINGEN./C.E.              
003600*                                                                         
003700                                                                          
003800     SKIP3                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP2                                                                
004100 INPUT-OUTPUT SECTION.                                                    
004200                                                                          
004300 FILE-CONTROL.                                                            
004400     SKIP2                                                                
004500*          --- UNICODE-TEXT                                               
004600     SELECT W15205                     ASSIGN TO W15210D1.                
004700     SKIP2                                                                
004800*          --- EBCDIC-TEXT                                                
004900     SELECT W15210                     ASSIGN TO W15210D2.                
005000*          --- FEL-POSTER TILL ÖVERSÄTTARE                                
005100     SELECT W15211                     ASSIGN TO W15210D3.                
005200     EJECT                                                                
005300 DATA DIVISION.                                                           
005400     SKIP3                                                                
005500 FILE SECTION.                                                            
005600     SKIP3                                                                
005700 FD  W15205                                                               
005800     RECORDING       V                                                    
005900     RECORD IS VARYING DEPENDING ON W15205-RCD-LEN                        
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200 01  FILLER   PIC X(496).                                                 
006300     SKIP3                                                                
006400 FD  W15210                                                               
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800*01  PBEN-POST -COPY W152PBEN -PRE  EBCD-  -L.                            
006900                                                                          
007000**** PRUB-POST  COPY W152PRUB -PRE  EBCD-  -L.                            
007200**** PFOT-POST  COPY W152PFOT -PRE  EBCD-  -L.                            
007400**** PTIL-POST  COPY W152PTIL -PRE  EBCD-  -L.                            
007500     EJECT                                                                
007600                                                                          
007700 FD  W15211                                                               
007800     LABEL RECORD STANDARD                                                
007900     BLOCK CONTAINS  0.                                                   
008000 01  W15211-POST               PIC X(80).                                 
008100                                                                          
008200     EJECT                                                                
008300 WORKING-STORAGE SECTION.                                                 
008400                                                                          
008500                                                                          
008600*    -- CHECKED BY WY2000                                                 
008700 77  IDPGM                       PIC X(8)    VALUE 'W1521000'.            
008800 77  JA                          PIC X       VALUE 'J'.                   
008900 77  NEJ                         PIC X       VALUE 'N'.                   
009000                                                                          
009100*    -- DELIMITER SOM INITIERAS I A-INIT                                  
009200 77  2-UCS-SPACE                 PIC X(4).                                
009300                                                                          
009400 77  4-EBCD-SPACE                PIC X(4)    VALUE SPACE.                 
009500                                                                          
009600 77  W15205-EOF-SW               PIC X       VALUE 'N'.                   
009700     88  END-OF-W15205                       VALUE 'J'.                   
009800 77  W15205-RCD-LEN              PIC 9(4)    COMP.                        
009900                                                                          
010000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010100 01  FILLER REDEFINES DAGENS-DATUM.                                       
010200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010500     EJECT                                                                
010600*    -- ARBETSAREA FÖR ATT LAGRA AKTUELLT SPRÅK I INPOSTEN                
010700*    -- (2-STÄLLIG ISO-KOD FÖR SPRÅK)                                     
010800 01  INX-SYSIN                   PIC X(40).                               
010900                                                                          
011000 01  WS-FILNAMN.                                                          
011100     03 FILNAMN-PREFIX           PIC X(2).                                
011101     03 FILNAMN-DATUMDEL         PIC X(6).                                
011102     03 FILNAMN-SUFFIXPUNKT      PIC X(1).                                
011110     03 WS-FILSPRAK              PIC X(2).                                
011111     03 FILLER                   PIC X(16).                               
011120                                                                          
011200 01  WIDSPRAK                    PIC X(2).                                
011300*    -- SPARAD LÄNGD PÅ TEXTEN I EN ENSKILD IN-POST                       
011400 77  WDIFAELT                    PIC 9(3)    COMP-3.                      
011500*    -- MAX TILLÅTEN LÄNGD PÅ TEXTEN I EN ENSKILD UT-POST                 
011600 77  WDIFAELT-UT                 PIC 9(3)    COMP-3.                      
011700                                                                          
011800*    -- RÄKNARE FÖR ANTAL POSTER MED SAMMA ID                             
011900 77  PN                          PIC S9(4)   COMP.                        
012000                                                                          
012100*    -- IN-AREA FÖR SAMMANSLAGNING/UPPDELNING AV TEXT                     
012200 77  W-TEXT                      PIC X(500).                              
012300                                                                          
012400*    -- TOTAL LÄNGD PÅ SIGNIFIKANT KONVERTERAD TEXT (TECONV-TO)           
012500 77  TOT-LEN                     PIC S9(4)   COMP.                        
012600                                                                          
012700*    -- POINTERS TILL OLIKA POS I TECONV-TO                               
012800 77  STR-PTR                     PIC S9(4)   COMP.                        
012900 77  END-POS                     PIC S9(4)   COMP.                        
013000 77  START-POS                   PIC S9(4)   COMP.                        
013100 77  BREAK-POS                   PIC S9(4)   COMP.                        
013200*    -- LÄNGD PÅ TEXT SOM SKA FLYTTAS VID UPPDELNING I BITAR              
013300 77  SLEN                        PIC S9(4)   COMP.                        
013400*    -- ARBETSFÄLT                                                        
013500 77  N                           PIC S9(4)   COMP.                        
013600 77  M                           PIC S9(4)   COMP.                        
013610 77  O                           PIC S9(4)   COMP.                        
013700                                                                          
013800*    -- FLAGGA SOM VISAR MIXED-MODE STATUS                                
013900 77  MODE-SW                     PIC X.                                   
014000     88 SB-MODE                  VALUE 'S'.                               
014100     88 DB-MODE                  VALUE 'D'.                               
014200     EJECT                                                                
014300 01  DYNAMISKA-SUBPROGRAM.                                                
014400*                                                                         
014500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014700     03  WCNVUNJA                PIC X(8)    VALUE 'WCNVUNJA'.            
014800     03  WCNVUNKO                PIC X(8)    VALUE 'WCNVUNKO'.            
014900     03  WCNVUNRU                PIC X(8)    VALUE 'WCNVUNRU'.            
015000     03  WCNVUNSV                PIC X(8)    VALUE 'WCNVUNSV'.            
015100     03  WCNVUNTH                PIC X(8)    VALUE 'WCNVUNTH'.            
015200     03  WCNVUNZH                PIC X(8)    VALUE 'WCNVUNZH'.            
015300     03  WCNVUNTR                PIC X(8)    VALUE 'WCNVUNTR'.            
015400*  KONVERTERAR FRÅN UCS-2 TILL UTF8                                       
015500     03  WCNVUUTF                PIC X(8)    VALUE 'WCNVUUTF'.            
015600     SKIP2                                                                
015700*    --- PARAMETRAR TILL ABEND                                            
015800                                                                          
015900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016200     SKIP2                                                                
016300 01  SKIPPA-SW                   PIC X(1)    VALUE 'N'.                   
016400     88 BEARBETA-POST                        VALUE 'N'.                   
016500     88 SKIPPA-POST                          VALUE 'J'.                   
016600                                                                          
016700 01  FELTEXT.                                                             
016800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
016900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017000     EJECT                                                                
017100                                                                          
017200 01  FILLER             PIC X(16)    VALUE 'W15211-AREA'.                 
017300 01  W15211-AREA.                                                         
017400     03  W15211-ID-DEL.                                                   
017500       05 W15211-IDPTYP      PIC X(4)     VALUE SPACE.                    
017600       05 FILLER-1           PIC X        VALUE SPACE.                    
017700       05 W15211-DIFAELT     PIC 9(3)     VALUE ZERO.                     
017800       05 FILLER-2           PIC X        VALUE SPACE.                    
017900       05 W15211-IDLEXNR     PIC 9(7)     VALUE ZERO.                     
018000       05 FILLER-3           PIC X        VALUE SPACE.                    
018100       05 W15211-IDSPRAK     PIC X(2)     VALUE SPACE.                    
018200       05 FILLER-4           PIC X        VALUE SPACE.                    
018300     03  W15211-TEXT         PIC X(60)    VALUE SPACE.                    
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL POSTSUM                                          
018600                                                                          
018700*01  -COPY W0005   -PRE  POSTSUM-                                         
018800     EJECT                                                                
018900*    --- PARAMETRAR TILL WCNVUNXX                                         
019000                                                                          
019100*01  -COPY WCNVAREA                                                       
019200                                                                          
019300     EJECT                                                                
019400*    -- POINTERS VID UPPDELNING AV EN INLÄST POST                         
019500*    -- I URPRUNGLIGA POSTER.                                             
019600 77  BIN-PTR                     PIC S9(4)   COMP.                        
019700 77  UCS2-PTR                    PIC S9(4)   COMP.                        
019800                                                                          
019900*    --- BIN-AREAN INNEHÅLLER DATA LAGRAT I UNICODE OCH                   
020000*    --- SOM TRANSMITERATS MED FTP BINARY MODE.                           
020100*    --- ENSKILDA POSTER MÅSTE DÄRFÖR SEPARERAS GENOM ATT                 
020200*    --- SCANNA EFTER CR/LF. EN POST CAN SPÄNNA ÖVER FLERA                
020300*    --- "FYSISKA" LOGISKA RECORDS.                                       
020400*    --- CR = X'000D' ELLER X'0D00'                                       
020500*    --- LF = X'000A' ELLER X'0A00'                                       
020600 01  BIN-AREA-START              PIC X(24)   VALUE                        
020700                                 'BIN-AREA-START  '.                      
020800 01  BIN-AREA                    PIC X(500).                              
020900                                                                          
021000     EJECT                                                                
021100*    --- UCS2-AREAN INNEHÅLLER DATA LAGRAT I UNICODE                      
021200*    --- OCH SOM DÄRFÖR MÅSTE KODAS OM INNAN DET KAN TOLKAS.              
021300 01  UCS2-AREA-START             PIC X(24)   VALUE                        
021400                                 'UCS2-AREA-START  '.                     
021500     SKIP2                                                                
021600 01  UCS2-AREA.                                                           
021700     03  TYP0.                                                            
021800       05  TYP0-BYTEORDER        PIC X(2).                                
021900       05  TYP0-TEXT-DEL         PIC X(498).                              
022000     03  TYP1 REDEFINES TYP0.                                             
022100       05  TYP1-ID-DEL           PIC X(40).                               
022200       05  TYP1-TEXT-DEL         PIC X(460).                              
022300*    03  TYP2 REDEFINES TYP0.                                             
022400*      05  TYP2-ID-DEL           PIC X(56).                               
022500*      05  TYP2-TEXT-DEL         PIC X(444).                              
022600     EJECT                                                                
022700 01  EBCD-AREA-START             PIC X(24)   VALUE                        
022800                                 'EBCD-AREA-START  '.                     
022900     SKIP2                                                                
023000 01  EBCD-AREA.                                                           
023100     03  EBCD-AREA-0.                                                     
023200       05 EBCD-ID-DEL.                                                    
023300         07 EBCD-IDPTYP          PIC X(4).                                
023400         07 FILLER               PIC X(1).                                
023500         07 EBCD-DIFAELT         PIC 9(3).                                
023600         07 FILLER               PIC X(1).                                
023700         07 EBCD-ID-NR           PIC 9(7).                                
023800         07 FILLER               PIC X(1).                                
023900         07 EBCD-IDSPRAK         PIC X(2).                                
024000         07 FILLER               PIC X(1).                                
024100       05  FILLER                PIC X(200).                              
024200*    03  FILLER -COPY W152PBEN  -PRE EBCD-  -RED  EBCD-AREA-0             
024600     EJECT                                                                
024700 01  SPAR-AREA-START             PIC X(24)   VALUE                        
024800                                 'SPAR-AREA-START  '.                     
024900     SKIP2                                                                
025000*    -- SPARAD INLEDNING PÅ UCS2-POSTEN                                   
025100 01  SPAR-AREA.                                                           
025200     03 SPAR-ID-DEL              PIC X(40).                               
025300     EJECT                                                                
025400*    -- USED WHEN CONVERTING FROM UCS2 TO UTF8                            
025500 01  UCS2-LENG                   PIC S9(9)   COMP.                        
025600 01  UTF8-LENG                   PIC S9(9)   COMP.                        
025700     EJECT                                                                
025800 PROCEDURE DIVISION.                                                      
025900 MAIN SECTION.                                                            
026000     SKIP2                                                                
026100     PERFORM A-INIT                                                       
026300     IF NOT END-OF-W15205                                                 
026400       PERFORM S05-HAEMTA-EN-POST                                         
026500       PERFORM B-KOLLA-RENSA-BYTE-ORDER-MARK                              
026600     END-IF                                                               
026601                                                                          
026602     IF  END-OF-W15205                                                    
026610       DISPLAY 'END-OF-W15205 INNAN BEHANDLING. FEL!'                     
026611*      --- SKRIV EN RAPPORT-POST PÅ FILEN TILL CBG                        
026612       MOVE    TYP0             TO W15211-ID-DEL                          
026613       MOVE SPACE TO FILLER-1 FILLER-2 FILLER-3 FILLER-4                  
026614       STRING 'FELAKTIGT TECKENFORMAT INFIL'                              
026615            DELIMITED BY SIZE INTO W15211-TEXT                            
026616       PERFORM S11-SKRIV-CBGFIL-W15211                                    
026617                                                                          
026618       DISPLAY TYP0 '=' W15211-TEXT                                       
026619     END-IF                                                               
026620                                                                          
026700     PERFORM UNTIL END-OF-W15205                                          
026800       PERFORM C-EXTRAHERA-KONVERTERA-POSTID                              
026900                                                                          
027000*      -- LÄGG IHOP DATA FRÅN POSTER MED SAMMA ID                         
027100       PERFORM D-INIT-POSTGRUPP                                           
027200       PERFORM UNTIL END-OF-W15205 OR                                     
027300                     TYP1-ID-DEL NOT = SPAR-ID-DEL                        
027400         PERFORM E-LAEGG-IHOP-TEXT                                        
027500         PERFORM S05-HAEMTA-EN-POST                                       
027600       END-PERFORM                                                        
027700       IF EBCD-IDPTYP = 'PBEN'                                            
027800         PERFORM F-KONVERTERA-TEXT                                        
027900                                                                          
028000         IF BEARBETA-POST                                                 
028100*      -- DELA UPP TEXTEN (IGEN) I LÄNGDER SOM PASSAR DATABASERNA         
028200           PERFORM G-DELA-UPP-TEXT                                        
028300         END-IF                                                           
028600       END-IF                                                             
028700     END-PERFORM                                                          
028800                                                                          
028900     PERFORM Z-FINIT                                                      
029000                                                                          
029100     MOVE ZERO TO RETURN-CODE                                             
029200     GOBACK                                                               
029300     .                                                                    
029400     EJECT                                                                
029500 A-INIT SECTION.                                                          
029600     SKIP2                                                                
029700     OPEN INPUT  W15205                                                   
029800     OPEN OUTPUT W15210                                                   
029900     OPEN OUTPUT W15211                                                   
030000                                                                          
030100     ACCEPT INX-SYSIN FROM SYSIN                                          
030110                                                                          
030200     MOVE FUNCTION UPPER-CASE(INX-SYSIN) TO INX-SYSIN                     
030300     UNSTRING INX-SYSIN  DELIMITED BY '/'                                 
030400       INTO WS-FILNAMN                                                    
030500                                                                          
030600     DISPLAY ' ---- KÖRNINGENS SPRÅK ÄR ' WS-FILSPRAK                     
030700     DISPLAY ' ---- INFILEN HETER ' WS-FILNAMN                            
030800                                                                          
030900*    -- LÄS FÖRSTA POSTEN SOM FÖRBEREDELSE FÖR S05-HAEMTA-EN-POST         
031000     PERFORM S06-LAES-W15205                                              
031100                                                                          
031200     ACCEPT DAGENS-DATUM  FROM DATE                                       
031300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
031400                                                                          
031500*    -- REN TEXT SKA GENERERAS VID KONVERTERINGARNA,                      
031600*    -- INTE NÅGRA SGML TEXT-ENTITIES.                                    
031700     MOVE NEJ TO FLTXTENT                                                 
031800     .                                                                    
031900     EJECT                                                                
032000 B-KOLLA-RENSA-BYTE-ORDER-MARK SECTION.                                   
032100     SKIP2                                                                
032200*--  FÖRSTA TVÅ BYTEN I FILEN ANGER "BYTE ORDER" OCH SKA                  
032300*--  RENSAS BORT FRÅN FÖRSTA POSTEN.                                      
032400*--  FFFE ANGER OMVÄND ORDNING ("LEAST SIGNIFICANT BIT", INTEL/PC)        
032500*--  FEFF ANGER RÄTTVÄND ORDNING ("MOST SIGNIFICANT BIT",                 
032600*--  MOTOROLA/MAC, IBM STORDATOR)                                         
032700*--  ANTAG RÄTTVÄNT OM INGEN MARKERING FINNS.                             
032800*--  INITIERA VISSA LITERALER BEROENDE PÅ BYTE-ORDER                      
032910                                                                          
033000     EVALUATE TYP0-BYTEORDER                                              
033100     WHEN X'FFFE'                                                         
033200       MOVE 'L' TO KDBYTEORD                                              
033300       MOVE TYP0-TEXT-DEL TO TYP0                                         
033400       MOVE X'20002000' TO 2-UCS-SPACE                                    
033500     WHEN X'FEFF'                                                         
033600       MOVE 'M' TO KDBYTEORD                                              
033700       MOVE TYP0-TEXT-DEL TO TYP0                                         
033800       MOVE X'00200020' TO 2-UCS-SPACE                                    
033900     WHEN OTHER                                                           
034000       MOVE 'M' TO KDBYTEORD                                              
034100       MOVE X'00200020' TO 2-UCS-SPACE                                    
034200     END-EVALUATE                                                         
034300     .                                                                    
034400     EJECT                                                                
034500 C-EXTRAHERA-KONVERTERA-POSTID  SECTION.                                  
034600     SKIP2                                                                
034700*    -- KONVERTERA DEN INLEDANDE ID-DELEN AV UCS2-POSTEN TILL             
034800*    -- SVENSK EBCDIC FÖR ATT KUNNA TOLKA POSTENS INNEHÅLL.               
034900*    -- DEN ÄR 40 BYTES LÅNG.                                             
035000*    -- RESULTATET BLIR 30 BYTES LÅNGT SINGEL-BYTE EBCDIC-DATA.           
035100                                                                          
035200     MOVE TYP1-ID-DEL TO TECONV-FROM                                      
035300     MOVE 30          TO KVMAXTL                                          
035500     CALL WCNVUNSV USING WCNVAREA                                         
035600                                                                          
035700     MOVE TECONV-TO (1:30) TO EBCD-AREA                                   
035800     .                                                                    
035900     EJECT                                                                
036000 D-INIT-POSTGRUPP   SECTION.                                              
036100     SKIP2                                                                
036200*    -- STRING SKA BÖRJA I POSITION 1                                     
036300     MOVE 1 TO STR-PTR                                                    
036400*    -- INGA POSTER HITTILLS                                              
036500     MOVE 0 TO PN                                                         
036600*    -- INGEN TEXT HITTILLS                                               
036700     MOVE SPACE TO W-TEXT                                                 
036800                                                                          
036900*    -- SPARA GRUPPENS IDENTITET (OKONVERTERAD UCS2)                      
037000     MOVE TYP1-ID-DEL    TO SPAR-ID-DEL                                   
037100     MOVE FUNCTION UPPER-CASE(EBCD-IDSPRAK) TO EBCD-IDSPRAK               
037200                                                                          
037300     IF WS-FILSPRAK = 'DE' OR 'EN' OR 'ES' OR 'FI' OR 'FR'                
037400           OR 'IT' OR 'MS' OR 'NL' OR 'PT' OR 'US' OR 'TR'                
037500           OR 'JA' OR 'RU' OR 'ZH' OR 'KO' OR 'TH' OR 'SV'                
037510           OR 'PL'                                                        
037600       MOVE WS-FILSPRAK  TO EBCD-IDSPRAK                                  
037700     END-IF                                                               
037800     MOVE EBCD-IDSPRAK TO WIDSPRAK                                        
037900     MOVE EBCD-DIFAELT TO WDIFAELT                                        
038000     .                                                                    
038100     EJECT                                                                
038200 E-LAEGG-IHOP-TEXT  SECTION.                                              
038300     SKIP2                                                                
038400*    -- SÄTT IHOP TEXTEN FRÅN ALLA POSTER MED SAMMA ID                    
038500*    -- SKIPPA TRAILING BLANKS                                            
038600                                                                          
038700     MOVE ZERO TO N M                                                     
038800                                                                          
038900     INSPECT TYP1-TEXT-DEL                                                
039000        TALLYING M FOR CHARACTERS BEFORE INITIAL 2-UCS-SPACE              
039100     INSPECT TYP1-TEXT-DEL                                                
039200        TALLYING N FOR CHARACTERS BEFORE INITIAL 4-EBCD-SPACE             
039300     COMPUTE SLEN = FUNCTION MIN(N M)                                     
039400     IF SLEN > 0                                                          
039500       STRING TYP1-TEXT-DEL (1:SLEN) DELIMITED BY SIZE                    
039600       INTO W-TEXT  WITH POINTER STR-PTR                                  
039700     END-IF                                                               
039800                                                                          
039900*    -- RÄKNA ANTAL SAMMANSLAGNA POSTER                                   
040000     ADD 1 TO PN                                                          
040100     .                                                                    
040200     EJECT                                                                
040300 F-KONVERTERA-TEXT  SECTION.                                              
040400     SKIP2                                                                
040500*    -- KONVERTERA UCS2-TEXTEN TILL UTF-8 ELLER EBCDIC                    
040600*    -- BEROENDE PÅ SPRÅK.                                                
040700                                                                          
040800     MOVE W-TEXT TO TECONV-FROM                                           
040900     COMPUTE UCS2-LENG = STR-PTR - 1                                      
041000     EVALUATE EBCD-IDPTYP                                                 
041100       WHEN 'PBEN' MOVE 150 TO KVMAXTL                                    
041500     END-EVALUATE                                                         
041600                                                                          
041700     SET BEARBETA-POST TO TRUE                                            
041710     IF TECONV-FROM = SPACE                                               
041711       MOVE 'F' TO KDSVAR                                                 
041720     ELSE                                                                 
041800       EVALUATE WIDSPRAK ALSO EBCD-IDPTYP                                 
041900*        -- BENÄMNINAR PÅ EXOTISKA SPRÅK SKA FÖRBLI I UNICODE,            
042000*        -- MEN ÄNDRAS FRÅN UCS2 TILL UTF8                                
042100         WHEN 'JA' ALSO 'PBEN'                                            
042200         WHEN 'RU' ALSO 'PBEN'                                            
042300         WHEN 'ZH' ALSO 'PBEN'                                            
042400         WHEN 'CN' ALSO 'PBEN'                                            
042500         WHEN 'KO' ALSO 'PBEN'                                            
042600         WHEN 'TH' ALSO 'PBEN'                                            
042610         WHEN 'PL' ALSO 'PBEN'                                            
042700         WHEN 'TR' ALSO 'PBEN'                                            
042800               MOVE KVMAXTL TO UTF8-LENG                                  
042900               MOVE SPACE  TO TECONV-TO                                   
043000               CALL WCNVUUTF USING TECONV-FROM UCS2-LENG                  
043100                                   TECONV-TO UTF8-LENG                    
043101*           --- BESTÄM IFALL KONVERTERAD TEXT ÄR > 100 BYTES              
043102            MOVE 0 TO O                                                   
043110            INSPECT TECONV-TO                                             
043120            TALLYING O FOR CHARACTERS BEFORE INITIAL 4-EBCD-SPACE         
043130            IF O > 100                                                    
043131              MOVE 'T' TO KDSVAR                                          
043140            END-IF                                                        
043200*        WHEN 'JA' ALSO NOT 'PBEN'                                        
043300*              CALL WCNVUNJA USING WCNVAREA                               
043400*                                                                         
043500*        WHEN 'RU' ALSO NOT 'PBEN'                                        
043600*              CALL WCNVUNRU USING WCNVAREA                               
043700*                                                                         
043800*        WHEN 'ZH' ALSO NOT 'PBEN'                                        
043900*              CALL WCNVUNZH USING WCNVAREA                               
044000*                                                                         
044100*        WHEN 'KO' ALSO NOT 'PBEN'                                        
044200*              CALL WCNVUNKO USING WCNVAREA                               
044300*                                                                         
044400*        WHEN 'TH' ALSO NOT 'PBEN'                                        
044500*              CALL WCNVUNTH USING WCNVAREA                               
044600*                                                                         
044700*        WHEN 'TR' ALSO NOT 'PBEN'                                        
044800*              CALL WCNVUNTR USING WCNVAREA                               
044900*                                                                         
045000*        -- ALLA SPRÅK MED VÄSTERLÄNDSKA BOKSTÄVER                        
045100*        -- ÖVERSÄTTS TILL SVENSK EBCDIC                                  
045200         WHEN 'DE' ALSO ANY                                               
045300         WHEN 'EN' ALSO ANY                                               
045400         WHEN 'ES' ALSO ANY                                               
045500         WHEN 'FI' ALSO ANY                                               
045600         WHEN 'FR' ALSO ANY                                               
045700         WHEN 'IT' ALSO ANY                                               
045800         WHEN 'MS' ALSO ANY                                               
045900         WHEN 'NL' ALSO ANY                                               
046000         WHEN 'PT' ALSO ANY                                               
046100         WHEN 'SV' ALSO ANY                                               
046200         WHEN 'US' ALSO ANY                                               
046210*        WHEN 'PL' ALSO ANY                                               
046300                    CALL WCNVUNSV USING WCNVAREA                          
046400                                                                          
046500         WHEN OTHER                                                       
046600           MOVE 'S' TO KDSVAR                                             
046700*          --- FLYTTAR IN EN FIKTIV FELKOD INFÖR FELHANTERINGEN           
046800       END-EVALUATE                                                       
046900     END-IF                                                               
047000* --- FELHANTERING ---------------------------------                      
047100     IF KDSVAR NOT = SPACE                                                
047200       IF KDSVAR = 'S'                                                    
047300         SET SKIPPA-POST TO TRUE                                          
047400*        --- SKRIV EN RAPPORT-POST PÅ FILEN TILL CBG                      
047500         MOVE  EBCD-ID-DEL     TO W15211-ID-DEL                           
047600         MOVE SPACE TO FILLER-1 FILLER-2 FILLER-3 FILLER-4                
047700                                                                          
047800         STRING 'FELAKTIGT SPRÅK '                                        
047900           DELIMITED BY SIZE INTO W15211-TEXT                             
048000                                                                          
048100         PERFORM S11-SKRIV-CBGFIL-W15211                                  
048200                                                                          
048300         DISPLAY EBCD-ID-DEL ' ' W15211-TEXT                              
048400       END-IF                                                             
048500                                                                          
048600       IF KDSVAR = 'F'                                                    
048700         SET SKIPPA-POST TO TRUE                                          
048800                                                                          
048900*        --- SKRIV EN RAPPORT-POST PÅ FILEN TILL CBG                      
049000         MOVE  EBCD-ID-DEL        TO W15211-ID-DEL                        
049100         MOVE SPACE TO FILLER-1 FILLER-2 FILLER-3 FILLER-4                
049200         STRING 'OTILLÅTET TECKEN ' BEFEL                                 
049300              DELIMITED BY SIZE INTO W15211-TEXT                          
049400         PERFORM S11-SKRIV-CBGFIL-W15211                                  
049500                                                                          
049600         DISPLAY EBCD-ID-DEL ' ' W15211-TEXT                              
049700       END-IF                                                             
049800                                                                          
049900       IF KDSVAR = 'T'                                                    
050000*        --- SKRIV EN RAPPORT-POST PÅ FILEN TILL CBG                      
050100*        --- POSTEN BEHÖVER INTE SKIPPAS, DEN ÄR JU BARA FÖR KORT?        
050110*        --- D.V.S. DEN VAR FÖR LÅNG FÖR WDD311-BEARTEXT                  
050200         MOVE  EBCD-ID-DEL        TO W15211-ID-DEL                        
050300         MOVE SPACE TO FILLER-1 FILLER-2 FILLER-3 FILLER-4                
050400         STRING 'DATA TRUNKERAT ' BEFEL                                   
050500              DELIMITED BY SIZE INTO W15211-TEXT                          
050600         PERFORM S11-SKRIV-CBGFIL-W15211                                  
050700*                                                                         
050800         DISPLAY EBCD-ID-DEL  ' ' W15211-TEXT                             
050900       END-IF                                                             
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 G-DELA-UPP-TEXT SECTION.                                                 
051400     SKIP2                                                                
051500*    -- FÖR BENÄMNINGAR OCH TILLÄGGSTEXTER KAN ENDAST EN POST             
051600*    -- GENERERAS. FLYTTA HELA KONVERTERADE TEXTEN TILL UT-AREAN.         
051700*    -- FÖR RUBRIKER OCH FOTNÖTTER KAN TEXTEN DELAS UPP I MAX             
051800*    -- TRE POSTER:                                                       
051900*    -- DELA AV VID LÄMPLIGT SPACE ELLER KOMMA (SBCS)                     
052000*    -- ELLER VID NÄRMASTE TECKEN-GRÄNS (DBCS)                            
052100                                                                          
052200     MOVE ZERO TO TOT-LEN                                                 
052300     INSPECT FUNCTION REVERSE(TECONV-TO)                                  
052400       TALLYING TOT-LEN FOR LEADING SPACE                                 
052500     COMPUTE TOT-LEN = LENGTH OF TECONV-TO - TOT-LEN                      
052600                                                                          
052700                                                                          
052800     EVALUATE EBCD-IDPTYP                                                 
052900      WHEN  'PBEN'                                                        
053000        MOVE TECONV-TO TO EBCD-PBEN-BEARTEXT                              
053100        PERFORM S11-SKRIV-W15210                                          
053500      WHEN OTHER                                                          
053510        CONTINUE                                                          
065100      END-EVALUATE                                                        
065200     .                                                                    
065300     EJECT                                                                
065400 Z-FINIT SECTION.                                                         
065500     SKIP2                                                                
065600     CLOSE W15205                                                         
065700           W15210                                                         
065800           W15211                                                         
065900                                                                          
066000     MOVE 'S' TO POSTSUM-OPKOD                                            
066100     CALL POSTSUM USING POSTSUM-PARM                                      
066200     .                                                                    
066300     EJECT                                                                
066400 S05-HAEMTA-EN-POST SECTION.                                              
066500     SKIP2                                                                
066600*    INFILEN HAR TRANSMITERATS MED FTP BINARY MODE.                       
066700*    OCH EN POST LIGGER I BIN-AREA.                                       
066800*    ENSKILDA POSTER MÅSTE DÄRFÖR SEPARERAS GENOM ATT                     
066900*    SCANNA EFTER CR/LF. EN "LOGISK" POST KAN SPÄNNA ÖVER                 
067000*    FLERA FYSISKA IN-POSTER.                                             
067100*    VARIABELN BIN-PTR PEKAR PÅ AKTUELL POS I IN-POSTEN                   
067200*    VARIABELN UCS2-PTR ANGER POS PÅ FÖRSTA LEDIGA BYTE I                 
067300*    UCS2-AREA.                                                           
067400                                                                          
067500*    -- TÖM FÖRST UCS2-AREAN                                              
067600     MOVE SPACE TO UCS2-AREA                                              
067700     MOVE 1 TO UCS2-PTR                                                   
067800                                                                          
067900*    -- PLOCKA EN DUBBEL-BYTE I TAGET FRÅN INLÄSTA BIN-AREAN              
068000*    -- TILLS FILEN ÄR SLUT ELLER CR (I NÅN FORM)                         
068100     PERFORM S05A-HAEMTA-NAESTA-W15205-DB                                 
068200     PERFORM UNTIL END-OF-W15205                                          
068300     OR BIN-AREA (BIN-PTR:2) = X'0D00' OR X'000D'                         
068400*      -- FLYTTA EN DUBBEL-BYTE OM DET FINNS PLATS                        
068500       IF UCS2-PTR < LENGTH OF UCS2-AREA                                  
068600         MOVE BIN-AREA (BIN-PTR:2) TO UCS2-AREA (UCS2-PTR:2)              
068700         ADD 2 TO UCS2-PTR                                                
068800       END-IF                                                             
068900       PERFORM S05A-HAEMTA-NAESTA-W15205-DB                               
069000     END-PERFORM                                                          
069100                                                                          
069200*    -- SKIPPA FÖRBI CR (LF SKIPPAS VID NÄSTA ANROP)                      
069300     IF NOT END-OF-W15205                                                 
069400       PERFORM S05A-HAEMTA-NAESTA-W15205-DB                               
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 S05A-HAEMTA-NAESTA-W15205-DB  SECTION.                                   
069900     SKIP2                                                                
070000*    VARIABELN BIN-PTR PEKAR PÅ SENAST ANVÄNDA POS I IN-POSTEN            
070100*    OM POSTEN EJ ÄR ANVÄND ÄR BIN-PTR = -1                               
070200*    (EN DUBBEL-BYTE KAN EJ SPÄNNA ÖVER TVÅ POSTER)                       
070300                                                                          
070400*    -- PEKA PÅ NÄSTA DUBBEL-BYTE                                         
070500     ADD 2 TO BIN-PTR                                                     
070600                                                                          
070700     IF BIN-PTR >= W15205-RCD-LEN                                         
070800       PERFORM S06-LAES-W15205                                            
070900       MOVE 1 TO BIN-PTR                                                  
071000     END-IF                                                               
071100     .                                                                    
071200     EJECT                                                                
071300 S06-LAES-W15205  SECTION.                                                
071400     SKIP2                                                                
071500     READ W15205 INTO BIN-AREA                                            
071600     AT END                                                               
071700        MOVE HIGH-VALUE TO BIN-AREA                                       
071800        SET END-OF-W15205 TO TRUE                                         
071900                                                                          
072000     NOT AT END                                                           
072100        MOVE -1 TO BIN-PTR                                                
072200        MOVE 'W15205'   TO POSTSUM-FDNAMN                                 
072300        MOVE 'W15210D1' TO POSTSUM-DDNAMN2                                
072400        MOVE SPACE      TO POSTSUM-TRANSTYP                               
072500        CALL POSTSUM USING POSTSUM-PARM                                   
072600     END-READ                                                             
072700     .                                                                    
072800     EJECT                                                                
072900 S11-SKRIV-W15210 SECTION.                                                
073000     SKIP2                                                                
073100     EVALUATE EBCD-IDPTYP                                                 
073200     WHEN 'PBEN'                                                          
073300       WRITE EBCD-PBEN-POST FROM EBCD-PBEN-W152PBEN                       
073310                                                                          
073400*    WHEN 'PRUB'                                                          
073500*      WRITE EBCD-PRUB-POST FROM EBCD-PRUB-W152PRUB                       
073600*    WHEN 'PFOT'                                                          
073700*      WRITE EBCD-PFOT-POST FROM EBCD-PFOT-W152PFOT                       
073800*    WHEN 'PTIL'                                                          
073900*      WRITE EBCD-PTIL-POST FROM EBCD-PTIL-W152PTIL                       
073910                                                                          
074000     END-EVALUATE                                                         
074100                                                                          
074200     MOVE EBCD-IDPTYP TO POSTSUM-TRANSTYP                                 
074300     MOVE 'W15210' TO POSTSUM-FDNAMN                                      
074400     MOVE 'W15210D2' TO POSTSUM-DDNAMN2                                   
074500     CALL POSTSUM USING POSTSUM-PARM                                      
074600     .                                                                    
074700     EJECT                                                                
074800 S11-SKRIV-CBGFIL-W15211  SECTION.                                        
074900     SKIP2                                                                
075000     WRITE  W15211-POST FROM W15211-AREA                                  
075100                                                                          
075200      MOVE 'CBG '    TO POSTSUM-TRANSTYP                                  
075300      MOVE 'W15211' TO POSTSUM-FDNAMN                                     
075400      MOVE 'W15210D3' TO POSTSUM-DDNAMN2                                  
075500      CALL POSTSUM USING POSTSUM-PARM                                     
075600                                                                          
075700     .                                                                    
075800     EJECT                                                                
075900 S99-ABEND SECTION.                                                       
076000     SKIP2                                                                
076100     MOVE 'S' TO POSTSUM-OPKOD                                            
076200     CALL POSTSUM USING POSTSUM-PARM                                      
076300     CALL ABEND USING RKOD-ABEND                                          
076400     CLOSE W15205                                                         
076500           W15210                                                         
076600           W15211                                                         
076700     GOBACK                                                               
076800     .                                                                    
