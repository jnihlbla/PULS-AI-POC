000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4617600.                                                 
001000*AUTHOR.        ELAINE OCH KATARINA.                                      
001100*DATE-WRITTEN.  OCTOBER  1990.                                            
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (PERU)                              
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIS, RKA, RKF, RIR, RKB - RKE                                 
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
003500     SELECT W46176                       ASSIGN TO UT-S-W46176D1.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - UTFIL:                                           
003800*                        - -  FIL TILL VIPS                               
003900     SELECT W4614W                       ASSIGN TO UT-S-W46176D2.         
004000     SELECT W46196                       ASSIGN TO UT-S-W46176D3.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46176                                                               
004700     RECORDING      V                                                     
004800     BLOCK CONTAINS 0.                                                    
005100     SKIP2                                                                
005110*01  FILLER -COPY W461RIAN        -L.                                     
005120     SKIP2                                                                
005130*01  FILLER -COPY W461RIBN        -L.                                     
005140     SKIP2                                                                
005150*01  FILLER -COPY W461RICN        -L.                                     
005160     SKIP2                                                                
005170*01  FILLER -COPY W461RIDN        -L.                                     
005180     SKIP2                                                                
005190*01  FILLER -COPY W461RIEN        -L.                                     
005191     SKIP2                                                                
005192*01  FILLER -COPY W461RIFN        -L.                                     
005193     SKIP2                                                                
005194*01  FILLER -COPY W461RIGN        -L.                                     
005195     SKIP2                                                                
005196*01  FILLER -COPY W461RIHN        -L.                                     
005197     SKIP2                                                                
005198*01  FILLER -COPY W461RIIN        -L.                                     
005199     SKIP2                                                                
005200*01  FILLER -COPY W461RIJN        -L.                                     
005201     SKIP2                                                                
005202*01  FILLER -COPY W461RIK1        -L.                                     
005203     SKIP2                                                                
005204*01  FILLER -COPY W461RILN        -L.                                     
005205     SKIP2                                                                
005206*01  FILLER -COPY W461RIMN        -L.                                     
005207     SKIP2                                                                
005208*01  FILLER -COPY W461RINN        -L.                                     
005209     SKIP2                                                                
005210*01  FILLER -COPY W461RIO2        -L.                                     
005211     SKIP2                                                                
005212*01  FILLER -COPY W461RIPN        -L.                                     
005213     SKIP2                                                                
005214*01  FILLER -COPY W461RIQN        -L.                                     
005215     SKIP2                                                                
005216*01  FILLER -COPY W461RIRN        -L.                                     
005217     SKIP2                                                                
005218*01  FILLER -COPY W461RISN        -L.                                     
005219     SKIP2                                                                
005220*01  FILLER -COPY W461RITN        -L.                                     
005221     SKIP2                                                                
005222*01  FILLER -COPY W461RIUN        -L.                                     
005223     SKIP2                                                                
005224*01  FILLER -COPY W461RIWN        -L.                                     
005225     SKIP2                                                                
005226*01  FILLER -COPY W461RIXN        -L.                                     
005227     SKIP2                                                                
005228*01  FILLER -COPY W461RIYN        -L.                                     
005229     SKIP2                                                                
005230*01  FILLER -COPY W461RIZN        -L.                                     
005231     SKIP2                                                                
005232*01  FILLER -COPY W461RKAN        -L.                                     
005233     SKIP2                                                                
005234*01  FILLER -COPY W461RKBN        -L.                                     
005235     SKIP2                                                                
005236*01  FILLER -COPY W461RKCN        -L.                                     
005237     SKIP2                                                                
005238*01  FILLER -COPY W461RKDN        -L.                                     
005239     SKIP2                                                                
005240*01  FILLER -COPY W461RKEN        -L.                                     
005241     SKIP2                                                                
005242*01  FILLER -COPY W461RKFN        -L.                                     
005243     SKIP2                                                                
005244*01  FILLER -COPY W461RKGN        -L.                                     
005245     SKIP2                                                                
005246*01  FILLER -COPY W461RKHN        -L.                                     
005247     SKIP2                                                                
005248*01  FILLER -COPY W461RKIN        -L.                                     
005249     EJECT                                                                
005250 FD  W4614W                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005600     SKIP2                                                                
005700 FD  W46196                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS 0.                                                    
006000 01  UTPOST2                      PIC X(80).                              
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210                                                                          
006300*    -- CHECKED BY WY2000                                                 
006800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4617600'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W46176-EOF                  PIC X(1)    VALUE 'N'.                   
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
011960*03  FILLER  -COPY W461RIM2        -PRE W- -RED W-ARBAREA-X               
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
012062*03  FILLER  -COPY W461RIMN        -PRE O- -RED W-OLDAREA-X               
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
015400     OPEN INPUT W46176 OUTPUT W4614W W46196                               
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
017300     PERFORM S01-LAS-W46176                                               
017400     PERFORM UNTIL                                                        
017500      NOT ( W46176-EOF = NEJ )                                            
017600       IF W-RIF-IDPTYP = 'RKF' OR 'RIS' OR 'RKA' OR 'RIR' OR              
017610         'RKB' OR 'RKC' OR 'RKD' OR 'RKE' OR 'RIM'                        
017620         EVALUATE      W-RKB-IDPTYP                                       
017630                                                                          
017735                                                                          
017736           WHEN    'RIR'                                                  
017737                PERFORM B-RIR-AENDRA-TILL-OLD                             
017738                                                                          
017739           WHEN    'RIS'                                                  
017740                PERFORM B-RIS-AENDRA-TILL-OLD                             
017741                                                                          
017760           WHEN    'RKA'                                                  
017761                PERFORM B-RKA-AENDRA-TILL-OLD                             
017762                                                                          
017763           WHEN    'RKB'                                                  
017764                PERFORM B-RKB-AENDRA-TILL-OLD                             
017765                                                                          
017766           WHEN    'RKC'                                                  
017767                PERFORM B-RKC-AENDRA-TILL-OLD                             
017768                                                                          
017769           WHEN    'RKD'                                                  
017770                PERFORM B-RKD-AENDRA-TILL-OLD                             
017771                                                                          
017772           WHEN    'RKE'                                                  
017773                PERFORM B-RKE-AENDRA-TILL-OLD                             
017774                                                                          
017775           WHEN    'RKF'                                                  
017776                PERFORM B-RKF-AENDRA-TILL-OLD                             
017777                                                                          
017778           WHEN    'RIM'                                                  
017779                PERFORM B-RIM-AENDRA-TILL-OLD                             
017786                                                                          
017787         END-EVALUATE                                                     
017788         ADD     +1           TO W-ANT-POSTER                             
017789         WRITE   UTPOST   FROM W-OLDAREA                                  
017790         WRITE   UTPOST2  FROM W-OLDAREA                                  
017791       END-IF                                                             
017792       PERFORM S01-LAS-W46176                                             
017793     END-PERFORM                                                          
017794     PERFORM BB-SLUT-KORT                                                 
017795     .                                                                    
017796     EJECT                                                                
017797                                                                          
018063 B-RIR-AENDRA-TILL-OLD SECTION.                                           
018064                                                                          
018065     MOVE W-RIR-IDPTYP         TO O-RIR-IDPTYP                            
018066     MOVE W-RIR-IDDC(1:1)      TO O-RIR-KDCLAGER                          
018067     MOVE W-RIR-IDDISTR        TO O-RIR-IDDISTR                           
018068     MOVE W-RIR-IDKUNDNR       TO O-RIR-IDKUNDNR                          
018069     MOVE W-RIR-IDORDNR        TO O-RIR-IDORDNR                           
018070     MOVE W-RIR-BEVOLREF       TO O-RIR-BEVOLREF                          
018071     MOVE W-RIR-TIORDREG       TO O-RIR-TIORDREG                          
018072     .                                                                    
018073     EJECT                                                                
018074                                                                          
018075 B-RIS-AENDRA-TILL-OLD SECTION.                                           
018076                                                                          
018077     MOVE W-RIS-IDPTYP         TO O-RIS-IDPTYP                            
018078     MOVE W-RIS-IDARTNR        TO O-RIS-IDARTNR                           
018079     MOVE W-RIS-REKSIFFR       TO O-RIS-REKSIFFR                          
018080     MOVE W-RIS-IDFKNGRP       TO O-RIS-IDFKNGRP                          
018081     MOVE W-RIS-KDSRA          TO O-RIS-KDSRA                             
018082     MOVE W-RIS-KVQPACK-1      TO O-RIS-KVQPACK-1                         
018083     MOVE +1                   TO O-RIS-KDCLAGER                          
018084     MOVE W-RIS-KDARTURS       TO O-RIS-KDARTURS                          
018085     MOVE W-RIS-KDPRODSL       TO O-RIS-KDPRODSL                          
018086     MOVE W-RIS-VLARTNTO       TO O-RIS-VLARTNTO                          
018087     MOVE W-RIS-VKART          TO O-RIS-VKART                             
018088     MOVE W-RIS-KDVSOP         TO O-RIS-KDVSOP                            
018089     MOVE W-RIS-IDSTATNR       TO O-RIS-IDSTATNR                          
018090     MOVE W-RIS-PRARTBTO-EXP   TO O-RIS-PRARTBTO-EXP                      
018091     MOVE W-RIS-FLMILART       TO O-RIS-FLMILART                          
018092     MOVE W-RIS-KDSORT         TO O-RIS-KDSORT                            
018093     MOVE W-RIS-KDERS          TO O-RIS-KDERS                             
018094     MOVE W-RIS-KDBPSR         TO O-RIS-KDBPSR                            
018095     MOVE W-RIS-KDBBCL         TO O-RIS-KDBBCL                            
018096     MOVE W-RIS-IDLEVNR        TO O-RIS-IDLEVNR                           
018097     MOVE W-RIS-KDAGE          TO O-RIS-KDAGE                             
018098     .                                                                    
018099     EJECT                                                                
018100                                                                          
018157 B-RKA-AENDRA-TILL-OLD SECTION.                                           
018158                                                                          
018159     MOVE W-ARBAREA            TO W-OLDAREA                               
018160     .                                                                    
018161     EJECT                                                                
018162                                                                          
018163 B-RKB-AENDRA-TILL-OLD SECTION.                                           
018164                                                                          
018165     MOVE W-RKB-IDPTYP         TO O-RKB-IDPTYP                            
018166     MOVE W-RKB-IDDC(1:1)      TO O-RKB-KDCLAGER                          
018167     MOVE W-RKB-IDDISTR        TO O-RKB-IDDISTR                           
018168     MOVE W-RKB-IDKUNDNR       TO O-RKB-IDKUNDNR                          
018169     MOVE W-RKB-IDKNOTNR       TO O-RKB-IDKNOTNR                          
018170     MOVE W-RKB-TIM-KN         TO O-RKB-TIM-KN                            
018171     MOVE W-RKB-IDRAPPNR       TO O-RKB-IDLEVANM                          
018172     MOVE W-RKB-PREMBHNT       TO O-RKB-PREMBHNT                          
018173     MOVE W-RKB-PRFRAKT        TO O-RKB-PRFRAKT                           
018174     MOVE W-RKB-PRLEGKST       TO O-RKB-PRLEGKST                          
018175     MOVE W-RKB-PRFOERS        TO O-RKB-PRFOERS                           
018176     MOVE W-RKB-PRMOMS         TO O-RKB-PRMOMS                            
018177     .                                                                    
018178     EJECT                                                                
018179                                                                          
018180 B-RKC-AENDRA-TILL-OLD SECTION.                                           
018181                                                                          
018182     MOVE W-RKC-IDPTYP         TO O-RKC-IDPTYP                            
018183     MOVE W-RKC-IDFAKT         TO O-RKC-IDFAKT                            
018184     MOVE W-RKC-IDORDNR        TO O-RKC-IDORDNR                           
018185     MOVE W-RKC-IDRADNR        TO O-RKC-IDRADNR                           
018186     MOVE W-RKC-IDARTNR        TO O-RKC-IDARTNR                           
018187     MOVE W-RKC-REKSIFFR       TO O-RKC-REKSIFFR                          
018188     MOVE W-RKC-KDANMORS       TO O-RKC-KDANMORS                          
018189     MOVE W-RKC-KVKREANT       TO O-RKC-KVKREANT                          
018190     MOVE W-RKC-PRARTBTO       TO O-RKC-PRARTBTO                          
018191     MOVE W-RKC-IDKOLLI        TO O-RKC-IDKOLLI                           
018192     .                                                                    
018193     EJECT                                                                
018194                                                                          
018195 B-RKD-AENDRA-TILL-OLD SECTION.                                           
018196                                                                          
018197     MOVE W-RKD-IDPTYP         TO O-RKD-IDPTYP                            
018198     MOVE W-RKD-IDDISTR        TO O-RKD-IDDISTR                           
018199     MOVE W-RKD-IDKUNDNR       TO O-RKD-IDKUNDNR                          
018200     MOVE W-RKD-IDDC(1:1)      TO O-RKD-KDCLAGER                          
018201     MOVE W-RKD-IDRAPPNR       TO O-RKD-IDRAPPNR                          
018202     MOVE W-RKD-IDORDNR        TO O-RKD-IDORDNR                           
018203     MOVE W-RKD-IDKOLLI        TO O-RKD-IDKOLLI                           
018204     MOVE W-RKD-IDARTNR        TO O-RKD-IDARTNR                           
018205     MOVE W-RKD-REKSIFFR       TO O-RKD-REKSIFFR                          
018206     MOVE W-RKD-IDRADNR        TO O-RKD-IDRADNR                           
018207     MOVE W-RKD-KDKREBEH       TO O-RKD-KDKREBEH                          
018208     MOVE W-RKD-KDANMORS       TO O-RKD-KDANMORS                          
018209     MOVE W-RKD-KVLEVANM       TO O-RKD-KVLEVANM                          
018210     MOVE W-RKD-PRARTBTO       TO O-RKD-PRARTBTO                          
018211     MOVE W-RKD-FLSKROT        TO O-RKD-FLSKROT                           
018212     .                                                                    
018213     EJECT                                                                
018214                                                                          
018215 B-RKE-AENDRA-TILL-OLD SECTION.                                           
018216                                                                          
018217     MOVE W-RKE-IDPTYP         TO O-RKE-IDPTYP                            
018218     MOVE W-RKE-IDDISTR        TO O-RKE-IDDISTR                           
018219     MOVE W-RKE-IDKUNDNR       TO O-RKE-IDKUNDNR                          
018220     MOVE W-RKE-IDDC(1:1)      TO O-RKE-KDCLAGER                          
018221     MOVE W-RKE-IDRAPPNR       TO O-RKE-IDRAPPNR                          
018222     MOVE W-RKE-IDRADNR        TO O-RKE-IDRADNR                           
018223     MOVE W-RKE-IDARTNR        TO O-RKE-IDARTNR                           
018224     MOVE W-RKE-REKSIFFR       TO O-RKE-REKSIFFR                          
018225     MOVE W-RKE-TIRETILL       TO O-RKE-TIRETILL                          
018226     MOVE W-RKE-IDRAPPNR-002   TO O-RKE-IDRAPPNR-002                      
018227     .                                                                    
018228     EJECT                                                                
018229                                                                          
018230 B-RKF-AENDRA-TILL-OLD SECTION.                                           
018231                                                                          
018232     MOVE W-RKF-IDPTYP         TO O-RKF-IDPTYP                            
018233     MOVE W-RKF-IDDC(1:1)      TO O-RKF-KDCLAGER                          
018234     MOVE W-RKF-IDTABNR        TO O-RKF-IDTABNR                           
018235     MOVE W-RKF-IDARTNR        TO O-RKF-IDARTNR                           
018236     MOVE W-RKF-REKSIFFR       TO O-RKF-REKSIFFR                          
018237     .                                                                    
018238     EJECT                                                                
018239                                                                          
018240 B-RIM-AENDRA-TILL-OLD  SECTION.                                          
018241                                                                          
018242     MOVE W-RIM-IDPTYP         TO O-RIM-IDPTYP                            
018243     MOVE W-RIM-IDKUNDNR       TO O-RIM-IDKUNDNR                          
018244     MOVE W-RIM-IDORDNR        TO O-RIM-IDORDNR                           
018245     MOVE W-RIM-IDPRODNR       TO O-RIM-IDPRODNR                          
018246     MOVE W-RIM-TIORDREG       TO O-RIM-TIORDREG                          
018247     MOVE W-RIM-KDORDKL        TO O-RIM-KDORDKL                           
018248     MOVE W-RIM-BEKUNDRF       TO O-RIM-BEVOLREF                          
018249     MOVE W-RIM-BEVARREF       TO O-RIM-BEVARREF                          
018250     MOVE W-RIM-KDREFNOT       TO O-RIM-KDREFNOT                          
018251     MOVE W-RIM-KDFRAKT        TO O-RIM-KDFRAKT                           
018252     MOVE W-RIM-IDTRPBOT       TO O-RIM-IDTRPBOT                          
018253     MOVE W-RIM-IDTRPBON       TO O-RIM-IDTRPBON                          
018254     .                                                                    
018255     EJECT                                                                
018260                                                                          
018600 BA-START-KORT SECTION.                                                   
018700     SKIP2                                                                
018800     MOVE     'RI0'          TO START-IDPTYP                              
018900     MOVE     6785           TO START-IDDISTR                             
019000     MOVE     1              TO START-KDCLAGER                            
019100     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
019200*                                                                         
019300     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
019400*                                                                         
019500     DISPLAY  'START-KORT  ' START-W461RI0                                
019600     WRITE    UTPOST         FROM  START-W461RI0                          
019700     WRITE    UTPOST2        FROM  START-W461RI0                          
019800     .                                                                    
019900     SKIP2                                                                
020000 BB-SLUT-KORT SECTION.                                                    
020100     SKIP2                                                                
020200     MOVE     'RI9'          TO SLUT-IDPTYP                               
020300     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
020400*                                                                         
020500     WRITE    UTPOST         FROM  SLUT-W461RI9                           
020600     WRITE    UTPOST2        FROM  SLUT-W461RI9                           
020700     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
020800     .                                                                    
020900     SKIP2                                                                
021000 S01-LAS-W46176 SECTION.                                                  
021100     SKIP2                                                                
021200     READ   W46176 INTO W-ARBAREA                                         
021300     AT END MOVE JA TO W46176-EOF                                         
021400     END-READ                                                             
021500                                                                          
021600     IF W46176-EOF = NEJ                                                  
021700                                                                          
021800       MOVE 'W46176'            TO POSTSUM-FDNAMN                         
021900       MOVE 'W46176D1'          TO POSTSUM-DDNAMN2                        
022000       MOVE SPACE               TO POSTSUM-TRANSTYP                       
022100       CALL POSTSUM             USING POSTSUM-PARM                        
022200                                                                          
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700     SKIP2                                                                
022800                                                                          
022900     CLOSE W46176 W4614W W46196                                           
023000     SKIP2                                                                
023100*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023200*                                    SKRIVNA POSTER                       
023300                                                                          
023400     MOVE 'S' TO POSTSUM-OPKOD                                            
023500     CALL POSTSUM USING POSTSUM-PARM                                      
023600     .                                                                    
