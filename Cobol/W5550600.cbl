001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5550600.                                                
001300 AUTHOR.         RANDI BERG.                                              
001400 DATE-WRITTEN.   98/02/05.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*        PROGRAMMET TAR EMOT PARAMETERPOST FRÅN BILD 5162,                
002000*        MATCHAR MOT WDD3 (BENÄMNING) OCH SKAPAR EN EXTRAKTFIL            
002100*        MED SAMMA SÖKBEGREPP SOM I BILD 5162                             
002200*                                                                         
002301*        PROGRAMMET LÄSER      WLLOGA (WDL9)                              
002310*        PROGRAMMET LÄSER      WLBENA (WDD3B)                             
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200*          --- SYSIN FRÅN JCL                                             
003201     SELECT INDATA                     ASSIGN TO SYSIN.                   
003202                                                                          
003204*          --- UTFIL EXTRAKT                                              
003210     SELECT W55506                     ASSIGN TO W55506D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD INDATA                                                                
003803     LABEL RECORD STANDARD                                                
003804     RECORDING F                                                          
003805     BLOCK CONTAINS 0.                                                    
003806 01  INPOST                      PIC X(80).                               
003807                                                                          
003808 FD  W55506                                                               
003809     RECORDING       F                                                    
003810     BLOCK CONTAINS  0.                                                   
003811                                                                          
003820*01  POST -COPY W55506 -PRE W55506-  -L.                                  
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W5550600'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004410 77  INDATA-EOF                  PIC X       VALUE 'N'.                   
004420 77  POST-SKALL-MED-SW           PIC X       VALUE 'N'.                   
004430                                                                          
004440*    --- ARBETSFÄLT                                                       
004450 01  W-AREA-DATUM.                                                        
004491     03  W-DAREGDAT-KONV         PIC 9(8).                                
004492     03  W-TIKLOCK-KONV          PIC 9(9).                                
004493     03  W-TIKLOCK               PIC S9(9)   COMP-3.                      
004494                                                                          
004495                                                                          
004500     SKIP2                                                                
004510 01  FILLER                      PIC X(10)   VALUE 'INAREA'.              
004511 01  INAREA.                                                              
004520     03  PRM-IDARTNR             PIC 9(9).                                
004521     03  FILLER                  PIC X.                                   
004540     03  PRM-IDDC                PIC X(2).                                
004541     03  FILLER                  PIC X.                                   
004550     03  PRM-IDHUVTYP            PIC X(4).                                
004551     03  FILLER                  PIC X.                                   
004552     03  PRM-IDSUBTYP            PIC X(3).                                
004553     03  FILLER                  PIC X.                                   
004554     03  PRM-DAREGDAT-MAX        PIC 9(8).                                
004555     03  FILLER                  PIC X.                                   
004556     03  PRM-DAREGDAT-MIN        PIC 9(8).                                
004557     03  FILLER                  PIC X.                                   
004560     03  PRM-IDTRANS             PIC X(4).                                
004561     03  FILLER                  PIC X.                                   
004570     03  PRM-IDUSER              PIC X(7).                                
004571     03  FILLER                  PIC X.                                   
004572     03  PRM-IDLAND              PIC X(3).                                
004573     03  FILLER                  PIC X(25).                               
004580                                                                          
004590*    ARBETSAREA FÖR REFERENSFÄLT                                          
004591 01  SPAR-UREF.                                                           
004592     03  SPAR-UREF1                    PIC X(25)   VALUE SPACE.           
004593     03  SPAR-IDGMTREF REDEFINES SPAR-UREF1.                              
004594         05  SPAR-IDDISTR              PIC X(5).                          
004595         05  FILLER                    PIC X.                             
004596         05  SPAR-IDKUNDNR             PIC X(7).                          
004597         05  FILLER                    PIC X.                             
004598         05  SPAR-IDKUNDRF             PIC X(10).                         
004599         05  SPAR-IDKUNDRF-FILLER REDEFINES SPAR-IDKUNDRF.                
004600             07  SPAR-IDORDNR5         PIC X(5).                          
004601     03  SPAR-IDLEVREF-FILLER REDEFINES SPAR-UREF1.                       
004602         05  SPAR-IDLEVREF.                                               
004603             07  SPAR-IDLOPNRM         PIC X(9).                          
004604             07  FILLER                PIC X.                             
004605             07  SPAR-IDLEVNR          PIC X(5).                          
004606             07  FILLER                PIC X(9).                          
004607     03  SPAR-IDPRCREF-FILLER REDEFINES SPAR-UREF1.                       
004608         05  SPAR-IDPRCREF.                                               
004609             07  SPAR-IDPRCBAS         PIC X(3).                          
004610             07  FILLER                PIC X.                             
004611             07  SPAR-IDPRCVAR         PIC X.                             
004612             07  FILLER                PIC X.                             
004613             07  SPAR-IDUSER-IDPRCREF  PIC X(8).                          
004614             07  FILLER                PIC X(5).                          
004615     03  SPAR-IDLBREF-FILLER REDEFINES SPAR-UREF1.                        
004616         05  SPAR-IDLBREF.                                                
004617             07  SPAR-TIFAKT           PIC X(7).                          
004618             07  FILLER                PIC X.                             
004619             07  SPAR-IDLBBET          PIC X(12).                         
004620             07  FILLER                PIC X.                             
004621     03  SPAR-IDANSTNR-FILLER REDEFINES SPAR-UREF1.                       
004622         05  SPAR-IDANSTREF.                                              
004623             07  SPAR-IDANSTNR         PIC X(5).                          
004624             07  FILLER                PIC X(20).                         
004625     03  SPAR-IDDC-SEND-FILLER REDEFINES SPAR-UREF1.                      
004626         05  SPAR-IDDC-SEND            PIC X(2).                          
004627         05  FILLER                    PIC X(15).                         
004628                                                                          
004629     03  SPAR-UREF2                    PIC X(25)   VALUE SPACE.           
004630     03  SPAR-IDFAKT-FILLER REDEFINES SPAR-UREF2.                         
004631         05  SPAR-IDFAKT               PIC X(7).                          
004632         05  FILLER                    PIC X(14).                         
004633     03  SPAR-IDKOLLI-FILLER REDEFINES SPAR-UREF2.                        
004634         05  SPAR-IDKOLLI              PIC X(5).                          
004635         05  FILLER                    PIC X(15).                         
004636     03  SPAR-IDPRODREF-FILLER REDEFINES SPAR-UREF2.                      
004637         05  SPAR-IDPRODREF.                                              
004638             07  SPAR-IDPRODNR         PIC X(7).                          
004639             07  FILLER                PIC X.                             
004640             07  SPAR-IDPLKLST         PIC X(3).                          
004641             07  FILLER                PIC X(12).                         
004642     03  SPAR-IDFS-FILLER REDEFINES SPAR-UREF2.                           
004643         05  SPAR-IDFS             PIC X(8).                              
004644         05  FILLER                PIC X(10).                             
004645     03  SPAR-IDRAPPNR-FILLER REDEFINES SPAR-UREF2.                       
004646         05  SPAR-IDRAPPNR         PIC X(7).                              
004647         05  FILLER                PIC X(11).                             
004648     03  SPAR-IDAVINR-FILLER REDEFINES SPAR-UREF2.                        
004649         05  SPAR-IDAVINR          PIC X(7).                              
004650         05  FILLER                PIC X(14).                             
004651     03  SPAR-IDKR-FILLER REDEFINES SPAR-UREF2.                           
004652         05  SPAR-IDKR             PIC X(5).                              
004653         05  FILLER                PIC X(13).                             
004654     03  SPAR-IDCLEARREF-FILLER REDEFINES SPAR-UREF2.                     
004655         05  SPAR-IDCLEARREF       PIC X(7).                              
004656         05  FILLER                PIC X(14).                             
004657     EJECT                                                                
004658*    ---GENERELLA ARBETSFÄLT                                              
004659 01  WS-IDDISTR                  PIC 9(5).                                
004660 01  WS-IDKUNDNR                 PIC 9(7).                                
004661 01  WS-IDFAKT                   PIC 9(7).                                
004662 01  WS-IDKOLLI                  PIC 9(5).                                
004663 01  WS-IDPRODNR                 PIC 9(7).                                
004664 01  WS-IDPRCBAS                 PIC X(3).                                
004665 01  WS-IDUSER-IDPRCREF          PIC X(8).                                
004666 01  WS-IDPLKLST                 PIC 9(3).                                
004667 01  WS-IDORDNR5                 PIC 9(5).                                
004668 01  WS-IDKUNDRF                 PIC X(10).                               
004669 01  WS-IDRAPPNR                 PIC 9(7).                                
004671 01  WS-IDAVINR                  PIC 9(7).                                
004672 01  WS-IDKR                     PIC 9(5).                                
004673 01  WS-IDLOPNRM                 PIC 9(9).                                
004674 01  WS-IDFS                     PIC X(8).                                
004675 01  WS-TIFAKT                   PIC 9(7).                                
004676 01  WS-IDLBBET                  PIC X(12).                               
004677 01  WS-IDANSTNR                 PIC 9(5).                                
004678 01  WS-IDARTNR                  PIC 9(9).                                
004679 01  WS-IDCLEARREF               PIC 9(7).                                
004680 01  WS-IDDC-SEND                PIC X(2).                                
004681     EJECT                                                                
004690 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300     EJECT                                                                
005400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES DAGENS-DATUM.                                       
005600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005900     EJECT                                                                
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006410     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006420     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006430     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006501     EJECT                                                                
006502*   -COPY WMEDAREA                                                        
006503                                                                          
006525*    --- PARAMETRAR TILL W005INIT                                         
006526*                                                                         
006527 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
006528     SKIP3                                                                
006529*01 -COPY WMSGINIT                                                        
006530     EJECT                                                                
006533                                                                          
006534                                                                          
006535*    --- PARAMETRAR TILL POSTSUM                                          
006536*                                                                         
006540*01  -COPY W0005   -PRE  POSTSUM-                                         
006801     EJECT                                                                
006802 01  W55506-AREA-START             PIC X(24)   VALUE                      
006803                                             'W555-AREA-START'.           
006804     SKIP2                                                                
006805                                                                          
006810*01  -COPY W55506                                                         
006900*                                                                         
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007200     SKIP3                                                                
007300 01  NYCKLAR-TILL-DLI.                                                    
007400     03  W-WDL901KY-MIN-X.                                                
007401         05  W-IDARTNR-MIN  PIC S9(9) COMP-3  VALUE ZERO.                 
007402         05  W-DAREGDAT-MIN PIC 9(8)          VALUE ZERO.                 
007403         05  W-TIKLOCK-MIN  PIC S9(9) COMP-3  VALUE ZERO.                 
007404         05  W-IDSEKVNR-MIN PIC S9(3) COMP-3  VALUE ZERO.                 
007405     03  W-WDL901KY-MAX-X.                                                
007406         05  W-IDARTNR-MAX  PIC S9(9) COMP-3  VALUE 999999999.            
007407         05  W-DAREGDAT-MAX PIC 9(8)          VALUE 99999999.             
007408         05  W-TIKLOCK-MAX  PIC S9(9) COMP-3  VALUE 999999999.            
007409         05  W-IDSEKVNR-MAX PIC S9(3) COMP-3  VALUE 999.                  
007410     03  W-WDL901-OVRIGA-MIN-X.                                           
007411         05  W-IDDC-MIN     PIC X(2)          VALUE LOW-VALUE.            
007412         05  W-IDHUVTYP-MIN PIC X(4)          VALUE LOW-VALUE.            
007413         05  W-IDSUBTYP-MIN PIC X(3)          VALUE LOW-VALUE.            
007414         05  W-IDTRANS-MIN  PIC X(4)          VALUE LOW-VALUE.            
007415     03  W-WDL901-OVRIGA-MAX-X.                                           
007416         05  W-IDDC-MAX     PIC X(2)          VALUE HIGH-VALUE.           
007417         05  W-IDHUVTYP-MAX PIC X(4)          VALUE HIGH-VALUE.           
007418         05  W-IDSUBTYP-MAX PIC X(3)          VALUE HIGH-VALUE.           
007419         05  W-IDTRANS-MAX  PIC X(4)          VALUE HIGH-VALUE.           
007420                                                                          
007423     03  W-IDARTNR-WDD3-X.                                                
007424         05  W-IDARTNR-WDD3      PIC S9(9)  VALUE ZERO COMP-3.            
007425     03  W-IDSKYLT-X.                                                     
007430         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
007500     SKIP2                                                                
007600*    --- STATUS-KOD FRÅN IMS                                              
007700 01  STATUS-WS                   PIC XX.                                  
007800     88  SEGMENT-FINNS                       VALUE '  '.                  
007900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008200     88  IMS-EJ-OK                           VALUE 'XD'.                  
008300     SKIP2                                                                
008400 01  GODK-STATUSKODER.                                                    
008500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008600     SKIP3                                                                
008700 01  SSA1                        PIC X(192).                              
008800 01  SSA2                        PIC X(64).                               
008900     EJECT                                                                
009000*    --- IMS FUNKTIONSKODER                                               
009100*01  -COPY W0003                                                          
009200     EJECT                                                                
009400*    ---  DLI INPUT-OUTPUT AREA                                           
009500                                                                          
009601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
009602 01  DLI-IO-WLLOGA01.                                                     
009603*    03  -COPY WDL901                                                     
009604 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA01'.                    
009605 01  DLI-IO-WLBENA01.                                                     
009606*    03  -COPY WDD311                                                     
009607     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100                                                                          
010200*01  -COPY W0009   -PRE MSG-                                              
010301                                                                          
010302*01  -COPY W0008  -PRE LOGA-                                              
010303     05  FILLER                  PIC X.                                   
010304                                                                          
010305*01  -COPY W0008  -PRE BENA-                                              
010310     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING MSG-PCB LOGA-PCB BENA-PCB.                     
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING MSG-PCB LOGA-PCB BENA-PCB.                     
010800                                                                          
011000     SKIP2                                                                
011100     PERFORM A-INIT                                                       
011200                                                                          
011210     PERFORM IMS-GN-LOGA                                                  
011220                                                                          
011300     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
011400                   SEGMENT-SLUT                                           
011500             PERFORM B-BEARBETA                                           
011600             PERFORM IMS-GN-LOGA                                          
011700     END-PERFORM                                                          
011800                                                                          
011900                                                                          
012400     PERFORM Z-FINIT                                                      
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
013000 A-INIT SECTION.                                                          
013010                                                                          
013020     OPEN INPUT INDATA                                                    
013030     READ INDATA NEXT RECORD INTO INAREA                                  
013040          AT END MOVE JA TO INDATA-EOF                                    
013050     END-READ                                                             
013060     CLOSE INDATA                                                         
013070                                                                          
013080     UNSTRING INAREA DELIMITED BY SPACE INTO PRM-IDARTNR                  
013092                                             PRM-IDDC                     
013093                                             PRM-IDHUVTYP                 
013094                                             PRM-IDSUBTYP                 
013095                                             PRM-DAREGDAT-MAX             
013096                                             PRM-DAREGDAT-MIN             
013097                                             PRM-IDTRANS                  
013098                                             PRM-IDUSER                   
013099                                             PRM-IDLAND                   
013100                                                                          
013101     DISPLAY 'PRM-IDARTNR*******' PRM-IDARTNR                             
013102     DISPLAY 'PRM-DAREGDAT-MAX**' PRM-DAREGDAT-MAX                        
013103     DISPLAY 'PRM-DAREGDAT-MIN**' PRM-DAREGDAT-MIN                        
013104     DISPLAY 'PRM-IDDC**********' PRM-IDDC                                
013105     DISPLAY 'PRM-IDHUVTYP******' PRM-IDHUVTYP                            
013106     DISPLAY 'PRM-IDSUBTYP******' PRM-IDSUBTYP                            
013107     DISPLAY 'PRM-IDTRANS*******' PRM-IDTRANS                             
013108     DISPLAY 'PRM-IDUSER********' PRM-IDUSER                              
013109     DISPLAY 'PRM-IDLAND********' PRM-IDLAND                              
013110                                                                          
013111     MOVE PRM-IDARTNR      TO W-IDARTNR-MIN                               
013112                              W-IDARTNR-MAX                               
013113     MOVE PRM-DAREGDAT-MAX TO W-DAREGDAT-MAX                              
013114     MOVE PRM-DAREGDAT-MIN TO W-DAREGDAT-MIN                              
013115     IF PRM-IDDC  NOT = ALL '+'                                           
013116        MOVE PRM-IDDC      TO W-IDDC-MAX                                  
013117                              W-IDDC-MIN                                  
013118     END-IF                                                               
013119     IF PRM-IDHUVTYP NOT = ALL '+'                                        
013120        MOVE PRM-IDHUVTYP  TO W-IDHUVTYP-MAX                              
013121                              W-IDHUVTYP-MIN                              
013122     END-IF                                                               
013123     IF PRM-IDSUBTYP NOT = ALL '+'                                        
013124        MOVE PRM-IDSUBTYP  TO W-IDSUBTYP-MAX                              
013125                              W-IDSUBTYP-MIN                              
013126     END-IF                                                               
013127     IF PRM-IDTRANS NOT = ALL '+'                                         
013128        MOVE PRM-IDTRANS   TO W-IDTRANS-MAX                               
013129                              W-IDTRANS-MIN                               
013130     END-IF                                                               
013131                                                                          
013132     IF PRM-IDLAND = 'SE'                                                 
013133        MOVE 'S' TO MED-IDSKYLT                                           
013134        MOVE 'S' TO W-IDSKYLT                                             
013135     ELSE                                                                 
013136        IF PRM-IDLAND = 'GB'                                              
013137            MOVE 'GB' TO MED-IDSKYLT                                      
013138            MOVE 'GB' TO W-IDSKYLT                                        
013139        ELSE                                                              
013140            MOVE 'US' TO MED-IDSKYLT                                      
013141            MOVE 'US' TO W-IDSKYLT                                        
013142        END-IF                                                            
013143     END-IF                                                               
013144     OPEN OUTPUT W55506                                                   
013145     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013146                                                                          
013150     MOVE ALL '+'           TO MSGI-WMSGINIT                              
013200     MOVE '001'             TO MSGI-KDCALL                                
013700     MOVE 'W555'            TO MSGI-IDTRANS                               
013900                                                                          
014100     .                                                                    
014200     EJECT                                                                
014300 B-BEARBETA SECTION.                                                      
014301                                                                          
014302     PERFORM S01-RAEKNA-OM-DATUM-TID                                      
014303     IF W-DAREGDAT-KONV > 19980531                                        
014304       MOVE LOGG-IDARTNR TO W-IDARTNR-WDD3                                
014305       PERFORM IMS-GET-BENA                                               
014313                                                                          
014314       MOVE PRM-IDARTNR             TO EXT-IDARTNR                        
014315       MOVE ';'                     TO EXT-SEMI1                          
014316       MOVE TEXT-BEART              TO EXT-BEART                          
014317       MOVE ';'                     TO EXT-SEMI2                          
014318       MOVE W-DAREGDAT-KONV         TO EXT-DAREGDAT                       
014319       MOVE ';'                     TO EXT-SEMI3                          
014320*      INSPECT W-TIKLOCK-KONV REPLACING LEADING ZERO BY SPACE             
014321       MOVE W-TIKLOCK-KONV          TO EXT-TIKLOCK                        
014322       MOVE ';'                     TO EXT-SEMI4                          
014323       MOVE LOGG-IDSEKVNR           TO EXT-IDSEKVNR                       
014324       MOVE ';'                     TO EXT-SEMI5                          
014325       MOVE LOGG-IDDC               TO EXT-IDDC                           
014326       MOVE ';'                     TO EXT-SEMI6                          
014327       MOVE LOGG-IDHUVTYP           TO EXT-IDHUVTYP                       
014328       MOVE ';'                     TO EXT-SEMI7                          
014329       MOVE LOGG-IDSUBTYP           TO EXT-IDSUBTYP                       
014330       MOVE ';'                     TO EXT-SEMI8                          
014331       MOVE LOGG-IDPGM              TO EXT-IDPGM                          
014332       MOVE ';'                     TO EXT-SEMI9                          
014333       MOVE LOGG-IDTRANS            TO EXT-IDTRANS                        
014334       MOVE ';'                     TO EXT-SEMI10                         
014335       MOVE LOGG-KVART-SALDO        TO EXT-KVART-SALDO (2:7)              
014336       IF LOGG-KVART-SALDO < 0                                            
014337          MOVE '-' TO EXT-KVART-SALDO (1:1)                               
014338       END-IF                                                             
014339       MOVE ';'                     TO EXT-SEMI11                         
014340       MOVE LOGG-IDTECKEN-KVAKS-PAV TO EXT-IDTECKEN-KVAKS-PAV             
014341       MOVE ';'                     TO EXT-SEMI12                         
014342       MOVE LOGG-KVAKS-PAV          TO EXT-KVAKS-PAV (2:7)                
014343       IF LOGG-KVAKS-PAV < 0                                              
014344          MOVE '-' TO EXT-KVAKS-PAV (1:1)                                 
014345       END-IF                                                             
014346       MOVE ';'                     TO EXT-SEMI13                         
014347       MOVE LOGG-IDTECKEN-KVAKS     TO EXT-IDTECKEN-KVAKS                 
014348       MOVE ';'                     TO EXT-SEMI14                         
014349       MOVE LOGG-KVAKS              TO EXT-KVAKS (2:7)                    
014350       IF LOGG-KVAKS < 0                                                  
014351          MOVE '-' TO EXT-KVAKS (1:1)                                     
014352       END-IF                                                             
014353       MOVE ';'                     TO EXT-SEMI15                         
014354       MOVE LOGG-IDTECKEN-KVEFRS    TO EXT-IDTECKEN-KVEFRS                
014355       MOVE ';'                     TO EXT-SEMI16                         
014356       MOVE LOGG-KVEFRS             TO EXT-KVEFRS (2:7)                   
014357       IF LOGG-KVEFRS < 0                                                 
014358          MOVE '-' TO EXT-KVEFRS (1:1)                                    
014359       END-IF                                                             
014360       MOVE ';'                     TO EXT-SEMI17                         
014361       MOVE LOGG-IDTECKEN-KVLS      TO EXT-IDTECKEN-KVLS                  
014362       MOVE ';'                     TO EXT-SEMI18                         
014363       MOVE LOGG-KVLS               TO EXT-KVLS (2:7)                     
014364       IF LOGG-KVLS < 0                                                   
014365          MOVE '-' TO EXT-KVLS (1:1)                                      
014366       END-IF                                                             
014367       MOVE ';'                     TO EXT-SEMI19                         
014368       MOVE LOGG-IDUSER             TO EXT-IDUSER                         
014369       MOVE ';'                     TO EXT-SEMI20                         
014370       PERFORM BA-PREPARERA-REFERENSFAELT                                 
014371       MOVE SPAR-UREF1              TO EXT-REF1                           
014372       MOVE ';'                     TO EXT-SEMI21                         
014373       MOVE SPAR-UREF2              TO EXT-REF2                           
014374       MOVE ';'                     TO EXT-SEMI22                         
014375       MOVE LOGG-DAREGDAT-LADD      TO EXT-DAREGDAT-LADD                  
014376                                                                          
014377       PERFORM S11-SKRIV-W55506                                           
014378       MOVE SPACE TO EXT-W55506                                           
014379       MOVE SPACE TO SPAR-UREF                                            
014380                     LOGG-REF                                             
014381     END-IF                                                               
014382     .                                                                    
014383     EJECT                                                                
014384                                                                          
014385 BA-PREPARERA-REFERENSFAELT SECTION.                                      
014386                                                                          
014387     EVALUATE LOGG-IDPGM                                                  
014388     WHEN 'W3018200'                                                      
014390        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014400        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014500        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014600     WHEN 'W3018300'                                                      
014610        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014620        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014630        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014631        MOVE LOGG-IDKOLLI         TO WS-IDKOLLI                           
014632        INSPECT WS-IDKOLLI REPLACING LEADING ZERO BY SPACE                
014633        MOVE WS-IDKOLLI           TO SPAR-IDKOLLI                         
014634     WHEN 'W4030400'                                                      
014635        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014636        INSPECT WS-IDDISTR  REPLACING LEADING ZERO BY SPACE               
014637        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014638        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014639        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014640        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014641        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
014642        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
014643        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
014644        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014645        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014646        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014647     WHEN 'W4031300'                                                      
014648        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014649        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014650        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014651        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014652        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014653        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014654        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
014655        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
014656        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
014657        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014658        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014659        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014660     WHEN 'W4035900'                                                      
014661        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014662        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014663        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014664        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014665        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014666        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014667        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
014668        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
014669        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
014670        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014671        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014672        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014673     WHEN 'W4037100'                                                      
014674        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014675        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014676        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014677        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014678        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014679        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014680        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
014681        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
014682        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
014686     WHEN 'W4037500'                                                      
014687        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014688        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014689        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014690        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014691        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014692        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014693        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
014694        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
014695        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
014696        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014697        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014698        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014699        MOVE LOGG-IDPLKLST        TO WS-IDPLKLST                          
014700        INSPECT WS-IDPLKLST REPLACING LEADING ZERO BY SPACE               
014701        MOVE WS-IDPLKLST          TO SPAR-IDPLKLST                        
014702     WHEN 'W411DEAV'                                                      
014703        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014704        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014705        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014706        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014707        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014708        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014709        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014710        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014711        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014712        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014713        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014714        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014715        MOVE LOGG-IDPLKLST        TO WS-IDPLKLST                          
014716        INSPECT WS-IDPLKLST REPLACING LEADING ZERO BY SPACE               
014717        MOVE WS-IDPLKLST          TO SPAR-IDPLKLST                        
014718     WHEN 'W4039700'                                                      
014719        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014720        INSPECT WS-IDDISTR  REPLACING LEADING ZERO BY SPACE               
014721        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014722        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014723        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014724        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014725        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014726        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014727        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014728        MOVE LOGG-IDPLKLST        TO WS-IDPLKLST                          
014729        INSPECT WS-IDPLKLST REPLACING LEADING ZERO BY SPACE               
014730        MOVE WS-IDPLKLST          TO SPAR-IDPLKLST                        
014731        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014732        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014733        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014734     WHEN 'W4039800'                                                      
014735        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014736        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014737        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014738        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014739        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014740        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014741        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014742        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014743        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014744        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014745        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014746        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014747     WHEN 'W403AVSP'                                                      
014748        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014749        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014750        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014751        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014752        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014753        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014754        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014755        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014756        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014757        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014758        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014759        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014760     WHEN 'W4079200'                                                      
014761        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014762        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014763        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014764        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014765        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014766        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014767        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
014768        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
014769        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
014770     WHEN 'W4079700'                                                      
014771        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014772        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014773        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014774        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014775        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014776        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014777        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
014778        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
014779        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
014780     WHEN 'W5010600'                                                      
014781     WHEN 'WL017100'                                                      
014782        MOVE LOGG-UREF1           TO SPAR-UREF1                           
014783        MOVE SPACE                TO SPAR-UREF2                           
014784     WHEN 'W5010800'                                                      
014785     WHEN 'WL017200'                                                      
014786        MOVE LOGG-IDUSER-IDPRCREF TO WS-IDUSER-IDPRCREF                   
014787        INSPECT WS-IDUSER-IDPRCREF REPLACING LEADING ZERO BY SPACE        
014788        MOVE WS-IDUSER-IDPRCREF   TO SPAR-IDUSER-IDPRCREF                 
014789        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
014790        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
014791        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
014792     WHEN 'W5010900'                                                      
014793        MOVE LOGG-IDDC-SEND       TO WS-IDDC-SEND                         
014794        INSPECT WS-IDDC-SEND REPLACING LEADING ZERO BY SPACE              
014795        MOVE WS-IDDC-SEND         TO SPAR-IDDC-SEND                       
014796        MOVE LOGG-IDCLEARREF      TO WS-IDCLEARREF                        
014797        INSPECT WS-IDCLEARREF REPLACING LEADING ZERO BY SPACE             
014798        MOVE WS-IDCLEARREF        TO SPAR-IDCLEARREF                      
014799     WHEN 'W5015100'                                                      
014800        IF LOGG-REF NOT = SPACE                                           
014801          MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                        
014802          INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE             
014803          MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                      
014804          MOVE LOGG-IDAVINR         TO WS-IDAVINR                         
014805          INSPECT WS-IDAVINR REPLACING LEADING ZERO BY SPACE              
014806          MOVE WS-IDAVINR         TO SPAR-IDAVINR                         
014807        END-IF                                                            
014808     WHEN 'W5030800'                                                      
014809     WHEN 'WL017800'                                                      
014810        MOVE LOGG-UREF1           TO SPAR-UREF1                           
014811        MOVE LOGG-UREF2           TO SPAR-UREF2                           
014812     WHEN 'W6011C00'                                                      
014813        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
014814        MOVE LOGG-IDAVINR         TO WS-IDAVINR                           
014815        INSPECT WS-IDAVINR REPLACING LEADING ZERO BY SPACE                
014816        MOVE WS-IDAVINR           TO SPAR-IDAVINR                         
014817     WHEN 'W6011D00'                                                      
014818        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
014819        MOVE LOGG-IDKR            TO WS-IDKR                              
014820        INSPECT WS-IDKR REPLACING LEADING ZERO BY SPACE                   
014821        MOVE WS-IDKR              TO SPAR-IDKR                            
014822     WHEN 'W6011800'                                                      
014823        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
014824        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
014825        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
014826        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
014827        MOVE SPACE                TO SPAR-IDAVINR                         
014828     WHEN 'W6011900'                                                      
014829        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
014830        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
014831        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
014832     WHEN 'W6019200'                                                      
014833        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
014836        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
014837        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
014838        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
014839        MOVE LOGG-IDFS            TO WS-IDFS                              
014840        INSPECT WS-IDFS REPLACING LEADING ZERO BY SPACE                   
014841        MOVE WS-IDFS              TO SPAR-IDFS                            
014842     WHEN 'W6019300'                                                      
014843        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
014846        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
014847        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
014848        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
014849        MOVE LOGG-IDFS            TO WS-IDFS                              
014850        INSPECT WS-IDFS REPLACING LEADING ZERO BY SPACE                   
014851        MOVE WS-IDFS              TO SPAR-IDFS                            
014852     WHEN 'W6030100'                                                      
014853        MOVE LOGG-TIFAKT          TO WS-TIFAKT                            
014854        INSPECT WS-TIFAKT REPLACING LEADING ZERO BY SPACE                 
014855        MOVE WS-TIFAKT            TO SPAR-TIFAKT                          
014856        MOVE LOGG-IDLBBET         TO WS-IDLBBET                           
014857        INSPECT WS-IDLBBET REPLACING LEADING ZERO BY SPACE                
014858        MOVE WS-IDLBBET           TO SPAR-IDLBBET                         
014859        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014860        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014861        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014862     WHEN 'W6030200'                                                      
014863        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014864        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014865        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014866     WHEN 'W6030300'                                                      
014867        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014868        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014869        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014870        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014871        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014872        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014873        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014874        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014875        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014876     WHEN 'W6030900'                                                      
014877        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014878        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014879        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014880        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
014881        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
014882        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
014883        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014884        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014885        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014886     WHEN 'W0110200'                                                      
014887        MOVE SPACE                TO SPAR-UREF1                           
014888        MOVE SPACE                TO SPAR-UREF2                           
014889     WHEN 'W2170400'                                                      
014890        MOVE SPACE                TO SPAR-UREF1                           
014891        MOVE SPACE                TO SPAR-UREF2                           
014892     WHEN 'W3712300'                                                      
014893        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014894        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014895        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014896        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014897        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014898        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014899        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014900        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014901        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014902     WHEN 'W3712700'                                                      
014903        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014904        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014905        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014906        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014907        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014908        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014909        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014910        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014911        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014912     WHEN 'W3713300'                                                      
014913        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014914        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014915        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014916        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014917        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014918        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014919        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
014920        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
014921        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
014922     WHEN 'W4183500'                                                      
014923        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014924        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014925        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014926        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014927        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014928        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014929        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
014930        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
014931        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
014932     WHEN 'W4752A00'                                                      
014933        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014934        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014935        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014936        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014937        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014938        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014939        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014940        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014941        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014942        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014943        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014944        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014945     WHEN 'W4752B00'                                                      
014946        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014947        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014948        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014949        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014950        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014951        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014952        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014953        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014954        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014955     WHEN 'W4752000'                                                      
014956        MOVE SPACE                TO SPAR-UREF1                           
014957        MOVE SPACE                TO SPAR-UREF2                           
014958     WHEN 'W4752099' THRU 'W4752100'                                      
014959        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
014960        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
014961        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
014962        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
014963        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
014964        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
014965        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
014966        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
014967        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
014968        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
014969        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
014970        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
014971     WHEN 'W5610700'                                                      
014972        MOVE SPACE                TO SPAR-UREF1                           
014973        MOVE SPACE                TO SPAR-UREF2                           
014974     WHEN 'W6110600'                                                      
014975        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
014978        MOVE LOGG-IDFS            TO WS-IDFS                              
014979        INSPECT WS-IDFS REPLACING LEADING ZERO BY SPACE                   
014980        MOVE WS-IDFS              TO SPAR-IDFS                            
014981     END-EVALUATE                                                         
014982     .                                                                    
014983     EJECT                                                                
014984                                                                          
014985 Z-FINIT SECTION.                                                         
014986                                                                          
014987                                                                          
014988     CLOSE W55506                                                         
014989     SKIP2                                                                
014990     MOVE 'S' TO POSTSUM-OPKOD                                            
014991     CALL POSTSUM USING POSTSUM-PARM                                      
015000     .                                                                    
015101     EJECT                                                                
015102                                                                          
015103 S01-RAEKNA-OM-DATUM-TID SECTION.                                         
015104                                                                          
015106     COMPUTE W-DAREGDAT-KONV = LOGG-DAREGDAT-9KOMPL - 99999999            
015107     COMPUTE W-TIKLOCK       = LOGG-TIKLOCK-9KOMPL  - 999999999           
015108     MOVE    W-TIKLOCK TO W-TIKLOCK-KONV                                  
015111     .                                                                    
015112     EJECT                                                                
015113                                                                          
015114 S11-SKRIV-W55506 SECTION.                                                
015115     SKIP2                                                                
015116     WRITE W55506-POST FROM EXT-W55506                                    
015117                                                                          
015118     MOVE 'EXT' TO POSTSUM-TRANSTYP                                       
015119     MOVE 'W55506 ' TO POSTSUM-FDNAMN                                     
015120     MOVE 'W55506D1' TO POSTSUM-DDNAMN2                                   
015121     CALL POSTSUM USING POSTSUM-PARM                                      
015130     .                                                                    
015300     EJECT                                                                
015400* --- IMS SEKTIONER ---                                                   
015500                                                                          
015601     EJECT                                                                
015602 IMS-GN-LOGA SECTION.                                                     
015603                                                                          
015604     STRING 'WLLOGA01(WDL901KY>=' W-WDL901KY-MIN-X                        
015605                    '&WDL901KY<=' W-WDL901KY-MAX-X                        
015606                    '&IDDC    >=' W-IDDC-MIN                              
015607                    '&IDDC    <=' W-IDDC-MAX                              
015608                    '&IDHUVTYP>=' W-IDHUVTYP-MIN                          
015609                    '&IDHUVTYP<=' W-IDHUVTYP-MAX                          
015610                    '&IDSUBTYP>=' W-IDSUBTYP-MIN                          
015611                    '&IDSUBTYP<=' W-IDSUBTYP-MAX                          
015612                    '&IDTRANS >=' W-IDTRANS-MIN                           
015613                    '&IDTRANS <=' W-IDTRANS-MAX ')'                       
015614          DELIMITED BY SIZE INTO SSA1                                     
015615     MOVE '  GE' TO GODK-STATUSKODER                                      
015616     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
015617     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
015618     PERFORM IMS-STATUSKONTROLL                                           
015619     .                                                                    
015620     EJECT                                                                
015631 IMS-GET-BENA SECTION.                                                    
015632                                                                          
015633     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
015634          DELIMITED BY SIZE INTO SSA1                                     
015635     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
015636          DELIMITED BY SIZE INTO SSA2                                     
015637     MOVE '  GE' TO GODK-STATUSKODER                                      
015638     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1 SSA2             
015639     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
015640     PERFORM IMS-STATUSKONTROLL                                           
015641     .                                                                    
015642     EJECT                                                                
015800 IMS-STATUSKONTROLL SECTION.                                              
015900     SKIP2                                                                
016000     SET STATUS-IX TO 1                                                   
016100     SEARCH GODK-STATUS                                                   
016200       AT END                                                             
016300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016400           DELIMITED BY SIZE INTO FELTEXT                                 
016500         DISPLAY FELTEXT                                                  
016600         CALL FELLOG                                                      
016700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016800         CONTINUE                                                         
016900     END-SEARCH                                                           
017000     .                                                                    
