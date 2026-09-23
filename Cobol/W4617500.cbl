000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4617500.                                                 
001000*AUTHOR.        ELAINE.                                                   
001100*DATE-WRITTEN.  FEBRUARI 1991.                                            
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (BRASILIEN-PV)                      
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIC SKRIVS INTE                                               
002100*                                                                         
002200*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
002300*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
002400*        (ENDAST ENGELSK TEXT).                                           
002500*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*- - - - - - - - - - - - INFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W46175                       ASSIGN TO UT-S-W46175D1.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - UTFIL:                                           
003800*                        - -  FIL TILL VIPS                               
003900     SELECT W4615I                       ASSIGN TO UT-S-W46175D2.         
004000     SELECT W46195                       ASSIGN TO UT-S-W46175D3.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46175                                                               
004700     RECORDING      V                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
005000*01  FILLER -COPY W461RIAN        -L.                                     
005100     SKIP2                                                                
005110*01  FILLER -COPY W461RIBN        -L.                                     
005120     SKIP2                                                                
005130*01  FILLER -COPY W461RICN        -L.                                     
005140     SKIP2                                                                
005150*01  FILLER -COPY W461RIDN        -L.                                     
005160     SKIP2                                                                
005170*01  FILLER -COPY W461RIEN        -L.                                     
005180     SKIP2                                                                
005190*01  FILLER -COPY W461RIFN        -L.                                     
005191     SKIP2                                                                
005192*01  FILLER -COPY W461RIGN        -L.                                     
005193     SKIP2                                                                
005194*01  FILLER -COPY W461RIHN        -L.                                     
005195     SKIP2                                                                
005196*01  FILLER -COPY W461RIIN        -L.                                     
005197     SKIP2                                                                
005198*01  FILLER -COPY W461RIJN        -L.                                     
005199     SKIP2                                                                
005200*01  FILLER -COPY W461RIK1        -L.                                     
005201     SKIP2                                                                
005202*01  FILLER -COPY W461RILN        -L.                                     
005203     SKIP2                                                                
005204*01  FILLER -COPY W461RIMN        -L.                                     
005205     SKIP2                                                                
005206*01  FILLER -COPY W461RINN        -L.                                     
005207     SKIP2                                                                
005208*01  FILLER -COPY W461RIO2        -L.                                     
005209     SKIP2                                                                
005210*01  FILLER -COPY W461RIPN        -L.                                     
005211     SKIP2                                                                
005212*01  FILLER -COPY W461RIQN        -L.                                     
005213     SKIP2                                                                
005214*01  FILLER -COPY W461RIRN        -L.                                     
005215     SKIP2                                                                
005216*01  FILLER -COPY W461RISN        -L.                                     
005217     SKIP2                                                                
005218*01  FILLER -COPY W461RITN        -L.                                     
005219     SKIP2                                                                
005220*01  FILLER -COPY W461RIUN        -L.                                     
005221     SKIP2                                                                
005222*01  FILLER -COPY W461RIWN        -L.                                     
005223     SKIP2                                                                
005224*01  FILLER -COPY W461RIXN        -L.                                     
005225     SKIP2                                                                
005226*01  FILLER -COPY W461RIYN        -L.                                     
005227     SKIP2                                                                
005228*01  FILLER -COPY W461RIZN        -L.                                     
005229     SKIP2                                                                
005230*01  FILLER -COPY W461RKAN        -L.                                     
005231     SKIP2                                                                
005232*01  FILLER -COPY W461RKBN        -L.                                     
005233     SKIP2                                                                
005234*01  FILLER -COPY W461RKCN        -L.                                     
005235     SKIP2                                                                
005236*01  FILLER -COPY W461RKDN        -L.                                     
005237     SKIP2                                                                
005238*01  FILLER -COPY W461RKEN        -L.                                     
005239     SKIP2                                                                
005240*01  FILLER -COPY W461RKFN        -L.                                     
005241     SKIP2                                                                
005242*01  FILLER -COPY W461RKGN        -L.                                     
005243     SKIP2                                                                
005244*01  FILLER -COPY W461RKHN        -L.                                     
005245     SKIP2                                                                
005246*01  FILLER -COPY W461RKIN        -L.                                     
005247     EJECT                                                                
005250 FD  W4615I                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005600     SKIP2                                                                
005700 FD  W46195                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS 0.                                                    
006000 01  UTPOST2                      PIC X(80).                              
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210                                                                          
006300*    -- CHECKED BY WY2000                                                 
006800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4617500'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W46175-EOF                  PIC X(1)    VALUE 'N'.                   
008000     SKIP2                                                                
008100*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008200                                                                          
008300 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008400 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008500                                                                          
008600*- - - - - - - - - - - - - -                                              
008700                                                                          
008800     EJECT                                                                
008900 01  DAGENS-DATUM.                                                        
009000   03  DAGENS-DATUM-AR           PIC 9(2).                                
009100   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009200   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009300                                                                          
009400 01  DAGENS-TID.                                                          
009500   03  DAGENS-TID-TIM            PIC 9(2).                                
009600   03  DAGENS-TID-MIN            PIC 9(2).                                
009700   03  DAGENS-TID-SEK            PIC 9(2).                                
009800                                                                          
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010100   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010300     SKIP3                                                                
010400*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010500                                                                          
010600 01  RETURKODER.                                                          
010700   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010900   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011000     EJECT                                                                
011100*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011200                                                                          
011300******************************************************************        
011400                                                                          
011500 01  W-ARBAREA.                                                           
011600 03  W-ARBAREA-X                 PIC X(211).                              
011700     SKIP2                                                                
011800*                                                                         
011810*03  FILLER  -COPY W461RIAN        -PRE W- -RED W-ARBAREA-X               
011820     EJECT                                                                
011830*03  FILLER  -COPY W461RIBN        -PRE W- -RED W-ARBAREA-X               
011840     EJECT                                                                
011850*03  FILLER  -COPY W461RICN        -PRE W- -RED W-ARBAREA-X               
011860     EJECT                                                                
011870*03  FILLER  -COPY W461RIDN        -PRE W- -RED W-ARBAREA-X               
011880     EJECT                                                                
011890*03  FILLER  -COPY W461RIEN        -PRE W- -RED W-ARBAREA-X               
011891     EJECT                                                                
011892*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
011893     EJECT                                                                
011894*03  FILLER  -COPY W461RIGN        -PRE W- -RED W-ARBAREA-X               
011895     EJECT                                                                
011896*03  FILLER  -COPY W461RIHN        -PRE W- -RED W-ARBAREA-X               
011897     EJECT                                                                
011898*03  FILLER  -COPY W461RIIN        -PRE W- -RED W-ARBAREA-X               
011899     EJECT                                                                
011900*03  FILLER  -COPY W461RIJN        -PRE W- -RED W-ARBAREA-X               
011910     EJECT                                                                
011920*03  FILLER  -COPY W461RIK1        -PRE W- -RED W-ARBAREA-X               
011930     EJECT                                                                
011940*03  FILLER  -COPY W461RILN        -PRE W- -RED W-ARBAREA-X               
011950     EJECT                                                                
011960*03  FILLER  -COPY W461RIMN        -PRE W- -RED W-ARBAREA-X               
011970     EJECT                                                                
011980*03  FILLER  -COPY W461RINN        -PRE W- -RED W-ARBAREA-X               
011990     EJECT                                                                
011991*03  FILLER  -COPY W461RIO2        -PRE W- -RED W-ARBAREA-X               
011992     EJECT                                                                
011993*03  FILLER  -COPY W461RIPN        -PRE W- -RED W-ARBAREA-X               
011994     EJECT                                                                
011995*03  FILLER  -COPY W461RIQN        -PRE W- -RED W-ARBAREA-X               
011996     EJECT                                                                
011997*03  FILLER  -COPY W461RIRN        -PRE W- -RED W-ARBAREA-X               
011998     EJECT                                                                
011999*03  FILLER  -COPY W461RISN        -PRE W- -RED W-ARBAREA-X               
012000     EJECT                                                                
012001*03  FILLER  -COPY W461RITN        -PRE W- -RED W-ARBAREA-X               
012002     EJECT                                                                
012003*03  FILLER  -COPY W461RIUN        -PRE W- -RED W-ARBAREA-X               
012004     EJECT                                                                
012005*03  FILLER  -COPY W461RIWN        -PRE W- -RED W-ARBAREA-X               
012006     EJECT                                                                
012007*03  FILLER  -COPY W461RIXN        -PRE W- -RED W-ARBAREA-X               
012008     EJECT                                                                
012009*03  FILLER  -COPY W461RIYN        -PRE W- -RED W-ARBAREA-X               
012010     EJECT                                                                
012011*03  FILLER  -COPY W461RIZN        -PRE W- -RED W-ARBAREA-X               
012012     EJECT                                                                
012013*03  FILLER  -COPY W461RKAN        -PRE W- -RED W-ARBAREA-X               
012014     EJECT                                                                
012015*03  FILLER  -COPY W461RKBN        -PRE W- -RED W-ARBAREA-X               
012016     EJECT                                                                
012017*03  FILLER  -COPY W461RKCN        -PRE W- -RED W-ARBAREA-X               
012018     EJECT                                                                
012019*03  FILLER  -COPY W461RKDN        -PRE W- -RED W-ARBAREA-X               
012020     EJECT                                                                
012021*03  FILLER  -COPY W461RKEN        -PRE W- -RED W-ARBAREA-X               
012022     EJECT                                                                
012023*03  FILLER  -COPY W461RKFN        -PRE W- -RED W-ARBAREA-X               
012024     EJECT                                                                
012025*03  FILLER  -COPY W461RKGN        -PRE W- -RED W-ARBAREA-X               
012026     EJECT                                                                
012027*03  FILLER  -COPY W461RKHN        -PRE W- -RED W-ARBAREA-X               
012028     EJECT                                                                
012029*03  FILLER  -COPY W461RKIN        -PRE W- -RED W-ARBAREA-X               
012030     EJECT                                                                
012031                                                                          
012032********************* GAMLA UTSEENDET ****************************        
012033**                                                                        
012034 01  W-OLDAREA.                                                           
012035 03  W-OLDAREA-X                 PIC X(80).                               
012036     SKIP2                                                                
012037*                                                                         
012038*03  FILLER  -COPY W461RIA0        -PRE O- -RED W-OLDAREA-X               
012039     EJECT                                                                
012040*03  FILLER  -COPY W461RIB0        -PRE O- -RED W-OLDAREA-X               
012041     EJECT                                                                
012042*03  FILLER  -COPY W461RIC0        -PRE O- -RED W-OLDAREA-X               
012043     EJECT                                                                
012044*03  FILLER  -COPY W461RID0        -PRE O- -RED W-OLDAREA-X               
012045     EJECT                                                                
012046*03  FILLER  -COPY W461RIE0        -PRE O- -RED W-OLDAREA-X               
012047     EJECT                                                                
012048*03  FILLER  -COPY W461RIF0        -PRE O- -RED W-OLDAREA-X               
012049     EJECT                                                                
012050*03  FILLER  -COPY W461RIG0        -PRE O- -RED W-OLDAREA-X               
012051     EJECT                                                                
012052*03  FILLER  -COPY W461RIH0        -PRE O- -RED W-OLDAREA-X               
012053     EJECT                                                                
012054*03  FILLER  -COPY W461RII0        -PRE O- -RED W-OLDAREA-X               
012055     EJECT                                                                
012056*03  FILLER  -COPY W461RIJ0        -PRE O- -RED W-OLDAREA-X               
012057     EJECT                                                                
012058*03  FILLER  -COPY W461RIK0        -PRE O- -RED W-OLDAREA-X               
012059     EJECT                                                                
012060*03  FILLER  -COPY W461RIL0        -PRE O- -RED W-OLDAREA-X               
012061     EJECT                                                                
012062*03  FILLER  -COPY W461RIM0        -PRE O- -RED W-OLDAREA-X               
012063     EJECT                                                                
012064*03  FILLER  -COPY W461RIN0        -PRE O- -RED W-OLDAREA-X               
012065     EJECT                                                                
012066*03  FILLER  -COPY W461RIO0        -PRE O- -RED W-OLDAREA-X               
012067     EJECT                                                                
012068*03  FILLER  -COPY W461RIP0        -PRE O- -RED W-OLDAREA-X               
012069     EJECT                                                                
012070*03  FILLER  -COPY W461RIQ0        -PRE O- -RED W-OLDAREA-X               
012071     EJECT                                                                
012072*03  FILLER  -COPY W461RIR0        -PRE O- -RED W-OLDAREA-X               
012073     EJECT                                                                
012074*03  FILLER  -COPY W461RIS0        -PRE O- -RED W-OLDAREA-X               
012075     EJECT                                                                
012076*03  FILLER  -COPY W461RIT0        -PRE O- -RED W-OLDAREA-X               
012077     EJECT                                                                
012078*03  FILLER  -COPY W461RIU0        -PRE O- -RED W-OLDAREA-X               
012079     EJECT                                                                
012080*03  FILLER  -COPY W461RIW0        -PRE O- -RED W-OLDAREA-X               
012081     EJECT                                                                
012082*03  FILLER  -COPY W461RIX0        -PRE O- -RED W-OLDAREA-X               
012083     EJECT                                                                
012084*03  FILLER  -COPY W461RIY0        -PRE O- -RED W-OLDAREA-X               
012085     EJECT                                                                
012086*03  FILLER  -COPY W461RIZ0        -PRE O- -RED W-OLDAREA-X               
012087     EJECT                                                                
012088*03  FILLER  -COPY W461RKA0        -PRE O- -RED W-OLDAREA-X               
012089     EJECT                                                                
012090*03  FILLER  -COPY W461RKB0        -PRE O- -RED W-OLDAREA-X               
012091     EJECT                                                                
012092*03  FILLER  -COPY W461RKC0        -PRE O- -RED W-OLDAREA-X               
012093     EJECT                                                                
012094*03  FILLER  -COPY W461RKD0        -PRE O- -RED W-OLDAREA-X               
012095     EJECT                                                                
012096*03  FILLER  -COPY W461RKE0        -PRE O- -RED W-OLDAREA-X               
012097     EJECT                                                                
012098*03  FILLER  -COPY W461RKF0        -PRE O- -RED W-OLDAREA-X               
012099     EJECT                                                                
012100*03  FILLER  -COPY W461RKG0        -PRE O- -RED W-OLDAREA-X               
012101     EJECT                                                                
012102*03  FILLER  -COPY W461RKH0        -PRE O- -RED W-OLDAREA-X               
012103     EJECT                                                                
012104*03  FILLER  -COPY W461RKI0        -PRE O- -RED W-OLDAREA-X               
012105     EJECT                                                                
012106                                                                          
012107*01  -COPY W461RIFN        -PRE JFR-.                                     
012110     EJECT                                                                
012200*                             STARTKORT                                   
012300*01  -COPY W461RI0                                                        
012500     EJECT                                                                
012600*                             SLUTKORT                                    
012700*01  -COPY W461RI9                                                        
012900     EJECT                                                                
013000*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013100                                                                          
013200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013300     SKIP2                                                                
013400*01  -COPY WDATKORT                                                       
013600     EJECT                                                                
013700*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013800                                                                          
013900*01  -COPY W0005       -PRE  POSTSUM-.                                    
014100     EJECT                                                                
014200 PROCEDURE DIVISION.                                                      
014300     SKIP2                                                                
014400     PERFORM A-INIT                                                       
014500     PERFORM B-BEHANDLA                                                   
014600     PERFORM Z-FINIT                                                      
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000                                                                          
015100     SKIP3                                                                
015200 A-INIT SECTION.                                                          
015300     SKIP2                                                                
015400     OPEN INPUT W46175 OUTPUT W4615I W46195                               
015500     SKIP2                                                                
015600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015700     SKIP2                                                                
015800*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
015900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016000     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016100     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016200     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016300     SKIP2                                                                
016400*- - - - - - - - - - - - - - - - TID                                      
016500*                                                                         
016600     ACCEPT   DAGENS-TID FROM TIME                                        
016700     .                                                                    
016800     EJECT                                                                
016900                                                                          
017000 B-BEHANDLA SECTION.                                                      
017100     SKIP2                                                                
017200     PERFORM BA-START-KORT                                                
017300     PERFORM S01-LAS-W46175                                               
017400     PERFORM UNTIL                                                        
017500      NOT ( W46175-EOF = NEJ )                                            
017600       IF W-RIF-IDPTYP  = 'RIF'                                           
017700         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
017800         PERFORM S01-LAS-W46175                                           
017900       ELSE                                                               
018000         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018010           PERFORM B-RIF-AENDRA-TILL-OLD                                  
018100           ADD     +1           TO W-ANT-POSTER                           
018200           WRITE   UTPOST   FROM W-OLDAREA                                
018300           WRITE   UTPOST2  FROM W-OLDAREA                                
018400           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018500         END-IF                                                           
018600         IF W-RIF-IDPTYP = 'RIC'  OR  'RKO'                               
018700           PERFORM S01-LAS-W46175                                         
018800         ELSE                                                             
018810           EVALUATE    W-RIA-IDPTYP                                       
018820                                                                          
018830             WHEN  'RIA'                                                  
018840                  PERFORM B-RIA-AENDRA-TILL-OLD                           
018850                                                                          
018860             WHEN  'RIB'                                                  
018870                  PERFORM B-RIB-AENDRA-TILL-OLD                           
018880                                                                          
018890             WHEN  'RIC'                                                  
018891                  PERFORM B-RIC-AENDRA-TILL-OLD                           
018892                                                                          
018893             WHEN  'RID'                                                  
018894                  PERFORM B-RID-AENDRA-TILL-OLD                           
018895                                                                          
018896             WHEN  'RIE'                                                  
018897                  PERFORM B-RIE-AENDRA-TILL-OLD                           
018898                                                                          
018899             WHEN  'RIF'                                                  
018900                  PERFORM B-RIF-AENDRA-TILL-OLD                           
018901                                                                          
018902             WHEN  'RIG'                                                  
018903                  PERFORM B-RIG-AENDRA-TILL-OLD                           
018904                                                                          
018905             WHEN  'RIH'                                                  
018906                  PERFORM B-RIH-AENDRA-TILL-OLD                           
018907                                                                          
018908             WHEN  'RII'                                                  
018909                  PERFORM B-RII-AENDRA-TILL-OLD                           
018910                                                                          
018911             WHEN  'RIJ'                                                  
018912                  PERFORM B-RIJ-AENDRA-TILL-OLD                           
018913                                                                          
018914             WHEN  'RIK'                                                  
018915                  PERFORM B-RIK-AENDRA-TILL-OLD                           
018916                                                                          
018917             WHEN  'RIL'                                                  
018918                  PERFORM B-RIL-AENDRA-TILL-OLD                           
018919                                                                          
018920             WHEN  'RIM'                                                  
018921                  PERFORM B-RIM-AENDRA-TILL-OLD                           
018922                                                                          
018923             WHEN  'RIN'                                                  
018924                  PERFORM B-RIN-AENDRA-TILL-OLD                           
018925                                                                          
018926             WHEN  'RIO'                                                  
018927                  PERFORM B-RIO-AENDRA-TILL-OLD                           
018928                                                                          
018929             WHEN  'RIP'                                                  
018930                  PERFORM B-RIP-AENDRA-TILL-OLD                           
018931                                                                          
018932             WHEN  'RIQ'                                                  
018933                  PERFORM B-RIQ-AENDRA-TILL-OLD                           
018934                                                                          
018935             WHEN  'RIR'                                                  
018936                  PERFORM B-RIR-AENDRA-TILL-OLD                           
018937                                                                          
018938             WHEN  'RIS'                                                  
018939                  PERFORM B-RIS-AENDRA-TILL-OLD                           
018940                                                                          
018941             WHEN  'RIT'                                                  
018942                  PERFORM B-RIT-AENDRA-TILL-OLD                           
018943                                                                          
018944             WHEN  'RIU'                                                  
018945                  PERFORM B-RIU-AENDRA-TILL-OLD                           
018946                                                                          
018947             WHEN  'RIW'                                                  
018948                  PERFORM B-RIW-AENDRA-TILL-OLD                           
018949                                                                          
018950             WHEN  'RIX'                                                  
018951                  PERFORM B-RIX-AENDRA-TILL-OLD                           
018952                                                                          
018953             WHEN  'RIY'                                                  
018954                  PERFORM B-RIY-AENDRA-TILL-OLD                           
018955                                                                          
018956             WHEN  'RIZ'                                                  
018957                  PERFORM B-RIZ-AENDRA-TILL-OLD                           
018958                                                                          
018959             WHEN  'RKA'                                                  
018960                  PERFORM B-RKA-AENDRA-TILL-OLD                           
018961                                                                          
018962             WHEN  'RKB'                                                  
018963                  PERFORM B-RKB-AENDRA-TILL-OLD                           
018964                                                                          
018965             WHEN  'RKC'                                                  
018966                  PERFORM B-RKC-AENDRA-TILL-OLD                           
018967                                                                          
018968             WHEN  'RKD'                                                  
018969                  PERFORM B-RKD-AENDRA-TILL-OLD                           
018970                                                                          
018971             WHEN  'RKE'                                                  
018972                  PERFORM B-RKE-AENDRA-TILL-OLD                           
018973                                                                          
018974             WHEN  'RKF'                                                  
018975                  PERFORM B-RKF-AENDRA-TILL-OLD                           
018976                                                                          
018977             WHEN  'RKG'                                                  
018978                  PERFORM B-RKG-AENDRA-TILL-OLD                           
018979                                                                          
018980             WHEN  'RKH'                                                  
018981                  PERFORM B-RKH-AENDRA-TILL-OLD                           
018982                                                                          
018983             WHEN  'RKI'                                                  
018984                  PERFORM B-RKI-AENDRA-TILL-OLD                           
018985                                                                          
018986           END-EVALUATE                                                   
018987           ADD     +1           TO W-ANT-POSTER                           
018988           WRITE   UTPOST   FROM W-OLDAREA                                
018989           WRITE   UTPOST2  FROM W-OLDAREA                                
018990           PERFORM S01-LAS-W46175                                         
018991         END-IF                                                           
018992       END-IF                                                             
018993     END-PERFORM                                                          
018994     PERFORM BB-SLUT-KORT                                                 
018995     .                                                                    
018996     EJECT                                                                
018997                                                                          
018998 B-RIA-AENDRA-TILL-OLD SECTION.                                           
018999                                                                          
019000     MOVE W-RIA-IDPTYP       TO O-RIA-IDPTYP                              
019001     MOVE W-RIA-IDDC(1:1)    TO O-RIA-KDCLAGER                            
019002     MOVE W-RIA-IDDISTR      TO O-RIA-IDDISTR                             
019003     MOVE W-RIA-IDKUNDNR     TO O-RIA-IDKUNDNR                            
019004     MOVE W-RIA-IDORDNR      TO O-RIA-IDORDNR                             
019005     MOVE W-RIA-KDORDKL      TO O-RIA-KDORDKL                             
019006     MOVE W-RIA-IDARTNR      TO O-RIA-IDARTNR                             
019007     MOVE W-RIA-REKSIFFR     TO O-RIA-REKSIFFR                            
019008     MOVE W-RIA-BERADREF     TO O-RIA-BERADREF                            
019009     MOVE W-RIA-IDRONR       TO O-RIA-IDRONR                              
019010     MOVE W-RIA-BEVOLREF     TO O-RIA-BEVOLREF                            
019011     MOVE W-RIA-KVLEVART     TO O-RIA-KVLEVART                            
019012     MOVE W-RIA-KDRESTR      TO O-RIA-KDRESTR                             
019013     MOVE W-RIA-TIMM         TO O-RIA-TIMM                                
019014     MOVE W-RIA-TIDD         TO O-RIA-TIDD                                
019015     MOVE W-RIA-TIKLOCK      TO O-RIA-TIKLOCK                             
019016     .                                                                    
019017     EJECT                                                                
019018                                                                          
019019 B-RIB-AENDRA-TILL-OLD SECTION.                                           
019020                                                                          
019021     MOVE W-ARBAREA          TO W-OLDAREA                                 
019022     .                                                                    
019023     EJECT                                                                
019024                                                                          
019025 B-RIC-AENDRA-TILL-OLD SECTION.                                           
019026                                                                          
019027     MOVE W-ARBAREA          TO W-OLDAREA                                 
019028     .                                                                    
019029     EJECT                                                                
019030                                                                          
019031 B-RID-AENDRA-TILL-OLD SECTION.                                           
019032                                                                          
019033     MOVE W-RID-IDPTYP         TO O-RID-IDPTYP                            
019034     MOVE W-RID-IDDC(1:1)      TO O-RID-KDCLAGER                          
019035     MOVE W-RID-IDARTNR        TO O-RID-IDARTNR                           
019036     MOVE W-RID-REKSIFFR       TO O-RID-REKSIFFR                          
019037     MOVE W-RID-IDLOPNRE       TO O-RID-IDLOPNRE                          
019038     MOVE W-RID-IDKORTNR       TO O-RID-IDKORTNR                          
019039     MOVE W-RID-BERADREF       TO O-RID-BERADREF                          
019040     MOVE W-RID-IDRONR         TO O-RID-IDRONR                            
019041     MOVE W-RID-BEVOLREF       TO O-RID-BEVOLREF                          
019042     MOVE W-RID-KDRESTR        TO O-RID-KDRESTR                           
019043     MOVE W-RID-KDERS          TO O-RID-KDERS                             
019044     MOVE W-RID-KVBEART        TO O-RID-KVBEART                           
019045     MOVE W-RID-IDARTNR-TILLK  TO O-RID-IDARTNR-TILLK                     
019046     MOVE W-RID-REKSIFFR-TILLK TO O-RID-REKSIFFR-TILLK                    
019047     MOVE W-RID-KVBEART-TILLK  TO O-RID-KVBEART-TILLK                     
019048     MOVE W-RID-DIERS-KVOT     TO O-RID-DIERS-KVOT                        
019049     MOVE W-RID-KDERSUP        TO O-RID-KDERSUP                           
019050     MOVE W-RID-KDDSP          TO O-RID-KDDSP                             
019051     .                                                                    
019052     EJECT                                                                
019053                                                                          
019054 B-RIE-AENDRA-TILL-OLD SECTION.                                           
019055                                                                          
019056     MOVE W-RIE-IDPTYP         TO O-RIE-IDPTYP                            
019057     MOVE W-RIE-IDDC(1:1)      TO O-RIE-KDCLAGER                          
019058     MOVE W-RIE-IDARTNR        TO O-RIE-IDARTNR                           
019059     MOVE W-RIE-REKSIFFR       TO O-RIE-REKSIFFR                          
019060     MOVE W-RIE-IDLOPNRE       TO O-RIE-IDLOPNRE                          
019061     MOVE W-RIE-IDKORTNR       TO O-RIE-IDKORTNR                          
019062     MOVE W-RIE-BERADREF       TO O-RIE-BERADREF                          
019063     MOVE W-RIE-IDRONR         TO O-RIE-IDRONR                            
019064     MOVE W-RIE-BEVOLREF       TO O-RIE-BEVOLREF                          
019065     MOVE W-RIE-KDRESTR        TO O-RIE-KDRESTR                           
019066     MOVE W-RIE-KDERS          TO O-RIE-KDERS                             
019067     MOVE W-RIE-KVBEART        TO O-RIE-KVBEART                           
019068     MOVE W-RIE-IDARTNR-TILLK  TO O-RIE-IDARTNR-TILLK                     
019069     MOVE W-RIE-REKSIFFR-TILLK TO O-RIE-REKSIFFR-TILLK                    
019070     MOVE W-RIE-KVBEART-TILLK  TO O-RIE-KVBEART-TILLK                     
019071     MOVE W-RIE-DIERS-KVOT     TO O-RIE-DIERS-KVOT                        
019072     MOVE W-RIE-KDERSUP        TO O-RIE-KDERSUP                           
019073     MOVE W-RIE-KDDSP          TO O-RIE-KDDSP                             
019074     .                                                                    
019075     EJECT                                                                
019076                                                                          
019077 B-RIF-AENDRA-TILL-OLD SECTION.                                           
019078                                                                          
019079     MOVE JFR-RIF-IDPTYP       TO O-RIF-IDPTYP                            
019080     MOVE JFR-RIF-IDDC(1:1)    TO O-RIF-KDCLAGER                          
019081     MOVE JFR-RIF-IDARTNR      TO O-RIF-IDARTNR                           
019082     MOVE JFR-RIF-REKSIFFR     TO O-RIF-REKSIFFR                          
019083     MOVE JFR-RIF-IDLOPNRE     TO O-RIF-IDLOPNRE                          
019084     MOVE JFR-RIF-IDKORTNR     TO O-RIF-IDKORTNR                          
019085     MOVE JFR-RIF-BERADREF     TO O-RIF-BERADREF                          
019086     MOVE JFR-RIF-IDRONR       TO O-RIF-IDRONR                            
019087     MOVE JFR-RIF-BEVOLREF     TO O-RIF-BEVOLREF                          
019088     MOVE JFR-RIF-KDRESTR      TO O-RIF-KDRESTR                           
019089     MOVE JFR-RIF-KDERS        TO O-RIF-KDERS                             
019090     MOVE JFR-RIF-KVBEART      TO O-RIF-KVBEART                           
019091     MOVE JFR-RIF-BEERS        TO O-RIF-BEERS                             
019092     MOVE JFR-RIF-KDERSUP      TO O-RIF-KDERSUP                           
019093     MOVE JFR-RIF-KDDSP        TO O-RIF-KDDSP                             
019094     .                                                                    
019095     EJECT                                                                
019096                                                                          
019097 B-RIG-AENDRA-TILL-OLD SECTION.                                           
019098                                                                          
019099     MOVE W-RIG-IDPTYP         TO O-RIG-IDPTYP                            
019100     MOVE W-RIG-IDDC(1:1)      TO O-RIG-KDCLAGER                          
019101     MOVE W-RIG-IDARTNR        TO O-RIG-IDARTNR                           
019102     MOVE W-RIG-REKSIFFR       TO O-RIG-REKSIFFR                          
019103     MOVE W-RIG-BERADREF       TO O-RIG-BERADREF                          
019104     MOVE W-RIG-IDRONR         TO O-RIG-IDRONR                            
019105     MOVE W-RIG-BEVOLREF       TO O-RIG-BEVOLREF                          
019106     MOVE W-RIG-KDRESTR        TO O-RIG-KDRESTR                           
019107     MOVE W-RIG-KVBEART        TO O-RIG-KVBEART                           
019108     MOVE W-RIG-KVBEART-Q      TO O-RIG-KVBEART-Q                         
019109     MOVE W-RIG-KVQPACK-1      TO O-RIG-KVQPACK-1                         
019110     MOVE W-RIG-KDDSP          TO O-RIG-KDDSP                             
019111     MOVE W-RIG-TIMM           TO O-RIG-TIMM                              
019112     MOVE W-RIG-TIDD           TO O-RIG-TIDD                              
019113     MOVE W-RIG-TIKLOCK        TO O-RIG-TIKLOCK                           
019114     .                                                                    
019115     EJECT                                                                
019116                                                                          
019117 B-RIH-AENDRA-TILL-OLD SECTION.                                           
019118                                                                          
019119     MOVE W-RIH-IDPTYP         TO O-RIH-IDPTYP                            
019120     MOVE W-RIH-IDDC(1:1)      TO O-RIH-KDCLAGER                          
019121     MOVE W-RIH-IDARTNR        TO O-RIH-IDARTNR                           
019122     MOVE W-RIH-REKSIFFR       TO O-RIH-REKSIFFR                          
019123     MOVE W-RIH-BERADREF       TO O-RIH-BERADREF                          
019124     MOVE W-RIH-IDRONR         TO O-RIH-IDRONR                            
019125     MOVE W-RIH-BEVOLREF       TO O-RIH-BEVOLREF                          
019126     MOVE W-RIH-KDRESTR        TO O-RIH-KDRESTR                           
019127     MOVE W-RIH-KVBEART        TO O-RIH-KVBEART                           
019128     MOVE W-RIH-KVAVBART       TO O-RIH-KVAVBART                          
019129     MOVE W-RIH-KVRO           TO O-RIH-KVRO                              
019130     MOVE W-RIH-KDDSP          TO O-RIH-KDDSP                             
019131     MOVE W-RIH-TIDISPIN       TO O-RIH-TIDISPIN                          
019132     MOVE W-RIH-TIMM           TO O-RIH-TIMM                              
019133     MOVE W-RIH-TIDD           TO O-RIH-TIDD                              
019134     MOVE W-RIH-TIKLOCK        TO O-RIH-TIKLOCK                           
019135     .                                                                    
019136     EJECT                                                                
019137                                                                          
019138 B-RII-AENDRA-TILL-OLD SECTION.                                           
019139                                                                          
019140     MOVE W-RII-IDPTYP         TO O-RII-IDPTYP                            
019141     MOVE W-RII-IDDC(1:1)      TO O-RII-KDCLAGER                          
019142     MOVE W-RII-IDARTNR        TO O-RII-IDARTNR                           
019143     MOVE W-RII-REKSIFFR       TO O-RII-REKSIFFR                          
019144     MOVE W-RII-BERADREF       TO O-RII-BERADREF                          
019145     MOVE W-RII-IDRONR         TO O-RII-IDRONR                            
019146     MOVE W-RII-BEVOLREF       TO O-RII-BEVOLREF                          
019147     MOVE W-RII-KDRESTR        TO O-RII-KDRESTR                           
019148     MOVE W-RII-KVBEART        TO O-RII-KVBEART                           
019149     MOVE W-RII-KDDSP          TO O-RII-KDDSP                             
019150     MOVE W-RII-TIMM           TO O-RII-TIMM                              
019151     MOVE W-RII-TIDD           TO O-RII-TIDD                              
019152     MOVE W-RII-TIKLOCK        TO O-RII-TIKLOCK                           
019153     .                                                                    
019154     EJECT                                                                
019155                                                                          
019156 B-RIJ-AENDRA-TILL-OLD SECTION.                                           
019157                                                                          
019158     MOVE W-RIJ-IDPTYP         TO O-RIJ-IDPTYP                            
019159     MOVE W-RIJ-IDDC(1:1)      TO O-RIJ-KDCLAGER                          
019160     MOVE W-RIJ-IDDISTR        TO O-RIJ-IDDISTR                           
019161     MOVE W-RIJ-IDKUNDNR       TO O-RIJ-IDKUNDNR                          
019162     MOVE W-RIJ-IDORDNR        TO O-RIJ-IDORDNR                           
019163     MOVE W-RIJ-KDORDKL        TO O-RIJ-KDORDKL                           
019164     MOVE W-RIJ-IDARTNR        TO O-RIJ-IDARTNR                           
019165     MOVE W-RIJ-REKSIFFR       TO O-RIJ-REKSIFFR                          
019166     MOVE W-RIJ-KVBEART        TO O-RIJ-KVBEART                           
019167     .                                                                    
019168     EJECT                                                                
019169                                                                          
019170 B-RIK-AENDRA-TILL-OLD SECTION.                                           
019171                                                                          
019172     MOVE W-RIK-IDPTYP         TO O-RIK-IDPTYP                            
019173     MOVE W-RIK-IDDC(1:1)      TO O-RIK-KDCLAGER                          
019174     MOVE W-RIK-IDDISTR        TO O-RIK-IDDISTR                           
019175     MOVE W-RIK-KDFAKTYP       TO O-RIK-KDFAKTYP                          
019176     MOVE W-RIK-IDFAKT         TO O-RIK-IDFAKT                            
019177     MOVE W-RIK-TIFAKT         TO O-RIK-TIFAKT                            
019178     MOVE W-RIK-IDFRASED       TO O-RIK-IDFRASED                          
019179     MOVE W-RIK-SUFKTBEL       TO O-RIK-SUFKTBEL                          
019180     MOVE W-RIK-KDVALUTA       TO O-RIK-KDVALUTA                          
019181     MOVE W-RIK-PRKURS         TO O-RIK-PRKURS                            
019182     MOVE W-RIK-SUFKTUTL       TO O-RIK-SUFKTUTL                          
019183     MOVE W-RIK-KDFAKNOT       TO O-RIK-KDFAKNOT                          
019184     .                                                                    
019185     EJECT                                                                
019186                                                                          
019187 B-RIL-AENDRA-TILL-OLD SECTION.                                           
019188                                                                          
019189     MOVE W-ARBAREA            TO W-OLDAREA                               
019190     .                                                                    
019191     EJECT                                                                
019192                                                                          
019193 B-RIM-AENDRA-TILL-OLD SECTION.                                           
019194                                                                          
019195     MOVE W-RIM-IDPTYP         TO O-RIM-IDPTYP                            
019196     MOVE W-RIM-IDKUNDNR       TO O-RIM-IDKUNDNR                          
019197     MOVE W-RIM-IDORDNR        TO O-RIM-IDORDNR                           
019198     MOVE W-RIM-IDPRODNR       TO O-RIM-IDPRODNR                          
019199     MOVE W-RIM-TIORDREG       TO O-RIM-TIORDREG                          
019200     MOVE W-RIM-KDORDKL        TO O-RIM-KDORDKL                           
019201     MOVE W-RIM-BEVOLREF       TO O-RIM-BEVOLREF                          
019202     MOVE W-RIM-BEVARREF       TO O-RIM-BEVARREF                          
019203     MOVE W-RIM-KDREFNOT       TO O-RIM-KDREFNOT                          
019204     MOVE W-RIM-KDFRAKT        TO O-RIM-KDFRAKT                           
019205     MOVE W-RIM-IDTRPBOT       TO O-RIM-IDTRPBOT                          
019206     MOVE W-RIM-IDTRPBON       TO O-RIM-IDTRPBON                          
019207     .                                                                    
019208     EJECT                                                                
019209                                                                          
019210 B-RIN-AENDRA-TILL-OLD SECTION.                                           
019211                                                                          
019212     MOVE W-ARBAREA            TO W-OLDAREA                               
019213     .                                                                    
019214     EJECT                                                                
019215                                                                          
019216 B-RIO-AENDRA-TILL-OLD SECTION.                                           
019217                                                                          
019218     MOVE W-RIO-IDPTYP         TO O-RIO-IDPTYP                            
019219     MOVE W-RIO-IDORDNR        TO O-RIO-IDORDNR                           
019220     MOVE W-RIO-IDARTNR        TO O-RIO-IDARTNR                           
019221     MOVE W-RIO-REKSIFFR       TO O-RIO-REKSIFFR                          
019222     MOVE W-RIO-BERADREF       TO O-RIO-BERADREF                          
019223     MOVE W-RIO-KVBEART        TO O-RIO-KVBEART                           
019224     MOVE W-RIO-KVLEVART       TO O-RIO-KVLEVART                          
019225     MOVE W-RIO-RESERVG        TO O-RIO-RESERVG                           
019226     MOVE W-RIO-PRARTBTO-EXP   TO O-RIO-PRARTBTO-EXP                      
019227     MOVE W-RIO-PRARTNTO       TO O-RIO-PRARTNTO                          
019228     MOVE W-RIO-IDFKNGRP       TO O-RIO-IDFKNGRP                          
019229     MOVE W-RIO-KDPRODSL       TO O-RIO-KDPRODSL                          
019230     MOVE W-RIO-KDDSP          TO O-RIO-KDDSP                             
019231     MOVE W-RIO-KDVVKL         TO O-RIO-KDVVKL                            
019232     MOVE W-RIO-KDVRINFO       TO O-RIO-KDVRINFO                          
019233     MOVE W-RIO-FLINVEST       TO O-RIO-FLINVEST                          
019234     MOVE W-RIO-FLPRTILL       TO O-RIO-FLPRTILL                          
019235     MOVE W-RIO-FLDIRLEV       TO O-RIO-FLDIRLEV                          
019236     MOVE W-RIO-KDRABATT       TO O-RIO-KDRABATT                          
019237     .                                                                    
019238     EJECT                                                                
019239                                                                          
019240 B-RIP-AENDRA-TILL-OLD SECTION.                                           
019241                                                                          
019242     MOVE W-ARBAREA            TO W-OLDAREA                               
019243     .                                                                    
019244     EJECT                                                                
019245                                                                          
019246 B-RIQ-AENDRA-TILL-OLD SECTION.                                           
019247                                                                          
019248     MOVE W-RIQ-IDPTYP         TO O-RIQ-IDPTYP                            
019249     MOVE W-RIQ-IDDISTR        TO O-RIQ-IDDISTR                           
019250     MOVE W-RIQ-IDKUNDNR       TO O-RIQ-IDKUNDNR                          
019251     MOVE W-RIQ-IDORDNR        TO O-RIQ-IDORDNR                           
019252     MOVE W-RIQ-IDDC(1:1)      TO O-RIQ-KDCLAGER                          
019253     MOVE W-RIQ-KDFAKTYP       TO O-RIQ-KDFAKTYP                          
019254     MOVE W-RIQ-IDFAKT         TO O-RIQ-IDFAKT                            
019255     MOVE W-RIQ-TIFAKT         TO O-RIQ-TIFAKT                            
019256     MOVE W-RIQ-KDPALL         TO O-RIQ-KDPALL                            
019257     MOVE W-RIQ-KVPALL         TO O-RIQ-KVPALL                            
019258     MOVE W-RIQ-KVKRAG         TO O-RIQ-KVKRAG                            
019259     MOVE W-RIQ-KVLOCK         TO O-RIQ-KVLOCK                            
019260     .                                                                    
019261     EJECT                                                                
019262                                                                          
019263 B-RIR-AENDRA-TILL-OLD SECTION.                                           
019264                                                                          
019265     MOVE W-RIR-IDPTYP         TO O-RIR-IDPTYP                            
019266     MOVE W-RIR-IDDC(1:1)      TO O-RIR-KDCLAGER                          
019267     MOVE W-RIR-IDDISTR        TO O-RIR-IDDISTR                           
019268     MOVE W-RIR-IDKUNDNR       TO O-RIR-IDKUNDNR                          
019269     MOVE W-RIR-IDORDNR        TO O-RIR-IDORDNR                           
019270     MOVE W-RIR-BEVOLREF       TO O-RIR-BEVOLREF                          
019271     MOVE W-RIR-TIORDREG       TO O-RIR-TIORDREG                          
019272     .                                                                    
019273     EJECT                                                                
019274                                                                          
019275 B-RIS-AENDRA-TILL-OLD SECTION.                                           
019276                                                                          
019277     MOVE W-RIS-IDPTYP         TO O-RIS-IDPTYP                            
019278     MOVE W-RIS-IDARTNR        TO O-RIS-IDARTNR                           
019279     MOVE W-RIS-REKSIFFR       TO O-RIS-REKSIFFR                          
019280     MOVE W-RIS-IDFKNGRP       TO O-RIS-IDFKNGRP                          
019281     MOVE W-RIS-KDSRA          TO O-RIS-KDSRA                             
019282     MOVE W-RIS-KVQPACK-1      TO O-RIS-KVQPACK-1                         
019283     MOVE +1                   TO O-RIS-KDCLAGER                          
019284     MOVE W-RIS-KDARTURS       TO O-RIS-KDARTURS                          
019285     MOVE W-RIS-KDPRODSL       TO O-RIS-KDPRODSL                          
019286     MOVE W-RIS-VLARTNTO       TO O-RIS-VLARTNTO                          
019287     MOVE W-RIS-VKART          TO O-RIS-VKART                             
019288     MOVE W-RIS-KDVSOP         TO O-RIS-KDVSOP                            
019289     MOVE W-RIS-IDSTATNR       TO O-RIS-IDSTATNR                          
019290     MOVE W-RIS-PRARTBTO-EXP   TO O-RIS-PRARTBTO-EXP                      
019291     MOVE W-RIS-FLMILART       TO O-RIS-FLMILART                          
019292     MOVE W-RIS-KDSORT         TO O-RIS-KDSORT                            
019293     MOVE W-RIS-KDERS          TO O-RIS-KDERS                             
019294     MOVE W-RIS-KDBPSR         TO O-RIS-KDBPSR                            
019295     MOVE W-RIS-KDBBCL         TO O-RIS-KDBBCL                            
019296     MOVE W-RIS-IDLEVNR        TO O-RIS-IDLEVNR                           
019297     MOVE W-RIS-KDAGE          TO O-RIS-KDAGE                             
019298     .                                                                    
019299     EJECT                                                                
019300                                                                          
019301 B-RIT-AENDRA-TILL-OLD SECTION.                                           
019302                                                                          
019303     MOVE W-ARBAREA            TO W-OLDAREA                               
019304     .                                                                    
019305     EJECT                                                                
019306                                                                          
019307 B-RIU-AENDRA-TILL-OLD SECTION.                                           
019308                                                                          
019309     MOVE W-RIU-IDPTYP         TO O-RIU-IDPTYP                            
019310     MOVE W-RIU-IDDC(1:1)      TO O-RIU-KDCLAGER                          
019311     MOVE W-RIU-IDARTNR        TO O-RIU-IDARTNR                           
019312     MOVE W-RIU-REKSIFFR       TO O-RIU-REKSIFFR                          
019313     MOVE W-RIU-IDLOPNRE       TO O-RIU-IDLOPNRE                          
019314     MOVE W-RIU-IDKORTNR       TO O-RIU-IDKORTNR                          
019315     MOVE W-RIU-KDRO           TO O-RIU-KDRO                              
019316     .                                                                    
019317     EJECT                                                                
019318                                                                          
019319 B-RIW-AENDRA-TILL-OLD SECTION.                                           
019320                                                                          
019321     MOVE W-ARBAREA            TO W-OLDAREA                               
019322     .                                                                    
019323     EJECT                                                                
019324                                                                          
019325 B-RIX-AENDRA-TILL-OLD SECTION.                                           
019326                                                                          
019327     MOVE W-ARBAREA            TO W-OLDAREA                               
019328     .                                                                    
019329     EJECT                                                                
019330                                                                          
019331 B-RIY-AENDRA-TILL-OLD SECTION.                                           
019332                                                                          
019333     MOVE W-RIY-IDPTYP         TO O-RIY-IDPTYP                            
019334     MOVE W-RIY-IDDC(1:1)      TO O-RIY-KDCLAGER                          
019335     MOVE W-RIY-IDARTNR        TO O-RIY-IDARTNR                           
019336     MOVE W-RIY-REKSIFFR       TO O-RIY-REKSIFFR                          
019337     MOVE W-RIY-KVBEART        TO O-RIY-KVBEART                           
019338     MOVE W-RIY-PRARTNTO       TO O-RIY-PRARTNTO                          
019339     MOVE W-RIY-PRARTBTO-EXP   TO O-RIY-PRARTBTO-EXP                      
019340     MOVE W-RIY-BERADREF       TO O-RIY-BERADREF                          
019341     MOVE W-RIY-KDDSP          TO O-RIY-KDDSP                             
019342     MOVE W-RIY-KDPRODSL       TO O-RIY-KDPRODSL                          
019343     MOVE W-RIY-IDFKNGRP       TO O-RIY-IDFKNGRP                          
019344     MOVE W-RIY-FLINVEST       TO O-RIY-FLINVEST                          
019345     MOVE W-RIY-FLPRTILL       TO O-RIY-FLPRTILL                          
019346     MOVE W-RIY-FLABON         TO O-RIY-FLABON                            
019347     MOVE W-RIY-KDTPOTYP       TO O-RIY-KDTPOTYP                          
019348     .                                                                    
019349     EJECT                                                                
019350                                                                          
019351 B-RIZ-AENDRA-TILL-OLD SECTION.                                           
019352                                                                          
019353     MOVE W-ARBAREA            TO W-OLDAREA                               
019354     .                                                                    
019355     EJECT                                                                
019356                                                                          
019357 B-RKA-AENDRA-TILL-OLD SECTION.                                           
019358                                                                          
019359     MOVE W-ARBAREA            TO W-OLDAREA                               
019360     .                                                                    
019361     EJECT                                                                
019362                                                                          
019363 B-RKB-AENDRA-TILL-OLD SECTION.                                           
019364                                                                          
019365     MOVE W-RKB-IDPTYP         TO O-RKB-IDPTYP                            
019366     MOVE W-RKB-IDDC(1:1)      TO O-RKB-KDCLAGER                          
019367     MOVE W-RKB-IDDISTR        TO O-RKB-IDDISTR                           
019368     MOVE W-RKB-IDKUNDNR       TO O-RKB-IDKUNDNR                          
019369     MOVE W-RKB-IDKNOTNR       TO O-RKB-IDKNOTNR                          
019370     MOVE W-RKB-TIM-KN         TO O-RKB-TIM-KN                            
019371     MOVE W-RKB-IDRAPPNR       TO O-RKB-IDLEVANM                          
019372     MOVE W-RKB-PREMBHNT       TO O-RKB-PREMBHNT                          
019373     MOVE W-RKB-PRFRAKT        TO O-RKB-PRFRAKT                           
019374     MOVE W-RKB-PRLEGKST       TO O-RKB-PRLEGKST                          
019375     MOVE W-RKB-PRFOERS        TO O-RKB-PRFOERS                           
019376     MOVE W-RKB-PRMOMS         TO O-RKB-PRMOMS                            
019377     .                                                                    
019378     EJECT                                                                
019379                                                                          
019380 B-RKC-AENDRA-TILL-OLD SECTION.                                           
019381                                                                          
019382     MOVE W-RKC-IDPTYP         TO O-RKC-IDPTYP                            
019383     MOVE W-RKC-IDFAKT         TO O-RKC-IDFAKT                            
019384     MOVE W-RKC-IDORDNR        TO O-RKC-IDORDNR                           
019385     MOVE W-RKC-IDRADNR        TO O-RKC-IDRADNR                           
019386     MOVE W-RKC-IDARTNR        TO O-RKC-IDARTNR                           
019387     MOVE W-RKC-REKSIFFR       TO O-RKC-REKSIFFR                          
019388     MOVE W-RKC-KDANMORS       TO O-RKC-KDANMORS                          
019389     MOVE W-RKC-KVKREANT       TO O-RKC-KVKREANT                          
019390     MOVE W-RKC-PRARTBTO       TO O-RKC-PRARTBTO                          
019391     MOVE W-RKC-IDKOLLI        TO O-RKC-IDKOLLI                           
019392     .                                                                    
019393     EJECT                                                                
019394                                                                          
019395 B-RKD-AENDRA-TILL-OLD SECTION.                                           
019396                                                                          
019397     MOVE W-RKD-IDPTYP         TO O-RKD-IDPTYP                            
019398     MOVE W-RKD-IDDISTR        TO O-RKD-IDDISTR                           
019399     MOVE W-RKD-IDKUNDNR       TO O-RKD-IDKUNDNR                          
019400     MOVE W-RKD-IDDC(1:1)      TO O-RKD-KDCLAGER                          
019401     MOVE W-RKD-IDRAPPNR       TO O-RKD-IDRAPPNR                          
019402     MOVE W-RKD-IDORDNR        TO O-RKD-IDORDNR                           
019403     MOVE W-RKD-IDKOLLI        TO O-RKD-IDKOLLI                           
019404     MOVE W-RKD-IDARTNR        TO O-RKD-IDARTNR                           
019405     MOVE W-RKD-REKSIFFR       TO O-RKD-REKSIFFR                          
019406     MOVE W-RKD-IDRADNR        TO O-RKD-IDRADNR                           
019407     MOVE W-RKD-KDKREBEH       TO O-RKD-KDKREBEH                          
019408     MOVE W-RKD-KDANMORS       TO O-RKD-KDANMORS                          
019409     MOVE W-RKD-KVLEVANM       TO O-RKD-KVLEVANM                          
019410     MOVE W-RKD-PRARTBTO       TO O-RKD-PRARTBTO                          
019411     MOVE W-RKD-FLSKROT        TO O-RKD-FLSKROT                           
019412     .                                                                    
019413     EJECT                                                                
019414                                                                          
019415 B-RKE-AENDRA-TILL-OLD SECTION.                                           
019416                                                                          
019417     MOVE W-RKE-IDPTYP         TO O-RKE-IDPTYP                            
019418     MOVE W-RKE-IDDISTR        TO O-RKE-IDDISTR                           
019419     MOVE W-RKE-IDKUNDNR       TO O-RKE-IDKUNDNR                          
019420     MOVE W-RKE-IDDC(1:1)      TO O-RKE-KDCLAGER                          
019421     MOVE W-RKE-IDRAPPNR       TO O-RKE-IDRAPPNR                          
019422     MOVE W-RKE-IDRADNR        TO O-RKE-IDRADNR                           
019423     MOVE W-RKE-IDARTNR        TO O-RKE-IDARTNR                           
019424     MOVE W-RKE-REKSIFFR       TO O-RKE-REKSIFFR                          
019425     MOVE W-RKE-TIRETILL       TO O-RKE-TIRETILL                          
019426     MOVE W-RKE-IDRAPPNR-002   TO O-RKE-IDRAPPNR-002                      
019427     .                                                                    
019428     EJECT                                                                
019429                                                                          
019430 B-RKF-AENDRA-TILL-OLD SECTION.                                           
019431                                                                          
019432     MOVE W-RKF-IDPTYP         TO O-RKF-IDPTYP                            
019433     MOVE W-RKF-IDDC(1:1)      TO O-RKF-KDCLAGER                          
019434     MOVE W-RKF-IDTABNR        TO O-RKF-IDTABNR                           
019435     MOVE W-RKF-IDARTNR        TO O-RKF-IDARTNR                           
019436     MOVE W-RKF-REKSIFFR       TO O-RKF-REKSIFFR                          
019437     .                                                                    
019438     EJECT                                                                
019439                                                                          
019440 B-RKG-AENDRA-TILL-OLD SECTION.                                           
019441                                                                          
019442     MOVE W-RKG-IDPTYP         TO O-RKG-IDPTYP                            
019443     MOVE W-RKG-IDDISTR        TO O-RKG-IDDISTR                           
019444     MOVE W-RKG-IDKUNDNR       TO O-RKG-IDKUNDNR                          
019445     MOVE W-RKG-IDBYTRAP       TO O-RKG-IDBYTRAP                          
019446     MOVE W-RKG-TIREGDAT-GODK  TO O-RKG-TIREGDAT-GODK                     
019447     MOVE W-RKG-IDARTNR-OBJ    TO O-RKG-IDARTNR-OBJ                       
019448     MOVE W-RKG-IDTABNR        TO O-RKG-IDTABNR                           
019449     MOVE W-RKG-KVRETUR-GODK   TO O-RKG-KVRETUR-GODK                      
019450     MOVE W-RKG-KDBYTSTA-OBJ   TO O-RKG-KDBYTSTA-OBJ                      
019451     MOVE W-RKG-IDORDNR        TO O-RKG-IDORDNR                           
019452     MOVE W-RKG-KDBYTREF       TO O-RKG-KDBYTREF                          
019453     MOVE W-RKG-IDKUNDRF       TO O-RKG-IDKUNDRF                          
019454     MOVE W-RKG-IDBYTRAD       TO O-RKG-IDBYTRAD                          
019455     .                                                                    
019456     EJECT                                                                
019457                                                                          
019458 B-RKH-AENDRA-TILL-OLD SECTION.                                           
019459                                                                          
019460     MOVE W-ARBAREA            TO W-OLDAREA                               
019461     .                                                                    
019462     EJECT                                                                
019463                                                                          
019464 B-RKI-AENDRA-TILL-OLD SECTION.                                           
019465                                                                          
019466     MOVE W-ARBAREA            TO W-OLDAREA                               
019467     .                                                                    
019468     EJECT                                                                
019469                                                                          
019900 BA-START-KORT SECTION.                                                   
020000     SKIP2                                                                
020100     MOVE     'RI0'          TO START-IDPTYP                              
020200     MOVE     7070           TO START-IDDISTR                             
020300     MOVE     1              TO START-KDCLAGER                            
020400     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020500*                                                                         
020600     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020700*                                                                         
020800     DISPLAY  'START-KORT  ' START-W461RI0                                
020900     WRITE    UTPOST         FROM  START-W461RI0                          
021000     WRITE    UTPOST2        FROM  START-W461RI0                          
021100     .                                                                    
021200     SKIP2                                                                
021300 BB-SLUT-KORT SECTION.                                                    
021400     SKIP2                                                                
021500     MOVE     'RI9'          TO SLUT-IDPTYP                               
021600     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021700*                                                                         
021800     WRITE    UTPOST         FROM  SLUT-W461RI9                           
021900     WRITE    UTPOST2        FROM  SLUT-W461RI9                           
022000     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022100     .                                                                    
022200     SKIP2                                                                
022300 S01-LAS-W46175 SECTION.                                                  
022400     SKIP2                                                                
022500     READ   W46175 INTO W-ARBAREA                                         
022600     AT END MOVE JA TO W46175-EOF                                         
022700     END-READ                                                             
022800                                                                          
022900     IF W46175-EOF = NEJ                                                  
023000                                                                          
023100       MOVE 'W46175'            TO POSTSUM-FDNAMN                         
023200       MOVE 'W46175D1'          TO POSTSUM-DDNAMN2                        
023300       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023400       CALL POSTSUM             USING POSTSUM-PARM                        
023500                                                                          
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 Z-FINIT SECTION.                                                         
024000     SKIP2                                                                
024100                                                                          
024200     CLOSE W46175 W4615I W46195                                           
024300     SKIP2                                                                
024400*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024500*                                    SKRIVNA POSTER                       
024600                                                                          
024700     MOVE 'S' TO POSTSUM-OPKOD                                            
024800     CALL POSTSUM USING POSTSUM-PARM                                      
024900     .                                                                    
