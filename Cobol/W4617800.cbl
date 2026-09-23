000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4617800.                                                 
001000*AUTHOR.        KATARINA KYMMER                                           
001100*DATE-WRITTEN.  MARS 1990                                                 
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION: *****************                                          
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (SAUDI)                             
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIC SKALL EJ SKRIVAS                                          
002100*                                                                         
002200*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
002300*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
002400*        (ENDAST ENGELSK TEXT).                                           
002500*    ABENDKODER:                                                          
002600*                                                                         
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*- - - - - - - - - - - - INFIL:                                           
003500*                        - -  FIL TILL VIPS                               
003600     SELECT W46178                       ASSIGN TO UT-S-W46178D1.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - UTFIL:                                           
003900*                        - -  FIL TILL VIPS                               
004000     SELECT W46198                       ASSIGN TO UT-S-W46178D2.         
004100     SELECT W4614V                       ASSIGN TO UT-S-W46178D3.         
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W46178                                                               
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005200     SKIP2                                                                
005210*01  FILLER -COPY W461RIAN        -L.                                     
005220     SKIP2                                                                
005230*01  FILLER -COPY W461RIBN        -L.                                     
005240     SKIP2                                                                
005250*01  FILLER -COPY W461RICN        -L.                                     
005260     SKIP2                                                                
005270*01  FILLER -COPY W461RIDN        -L.                                     
005280     SKIP2                                                                
005290*01  FILLER -COPY W461RIEN        -L.                                     
005291     SKIP2                                                                
005292*01  FILLER -COPY W461RIFN        -L.                                     
005293     SKIP2                                                                
005294*01  FILLER -COPY W461RIGN        -L.                                     
005295     SKIP2                                                                
005296*01  FILLER -COPY W461RIHN        -L.                                     
005297     SKIP2                                                                
005298*01  FILLER -COPY W461RIIN        -L.                                     
005299     SKIP2                                                                
005300*01  FILLER -COPY W461RIJN        -L.                                     
005301     SKIP2                                                                
005302*01  FILLER -COPY W461RIK1        -L.                                     
005303     SKIP2                                                                
005304*01  FILLER -COPY W461RILN        -L.                                     
005305     SKIP2                                                                
005306*01  FILLER -COPY W461RIMN        -L.                                     
005307     SKIP2                                                                
005308*01  FILLER -COPY W461RINN        -L.                                     
005309     SKIP2                                                                
005310*01  FILLER -COPY W461RIO2        -L.                                     
005311     SKIP2                                                                
005312*01  FILLER -COPY W461RIPN        -L.                                     
005313     SKIP2                                                                
005314*01  FILLER -COPY W461RIQN        -L.                                     
005315     SKIP2                                                                
005316*01  FILLER -COPY W461RIRN        -L.                                     
005317     SKIP2                                                                
005318*01  FILLER -COPY W461RISN        -L.                                     
005319     SKIP2                                                                
005320*01  FILLER -COPY W461RITN        -L.                                     
005321     SKIP2                                                                
005322*01  FILLER -COPY W461RIUN        -L.                                     
005323     SKIP2                                                                
005324*01  FILLER -COPY W461RIWN        -L.                                     
005325     SKIP2                                                                
005326*01  FILLER -COPY W461RIXN        -L.                                     
005327     SKIP2                                                                
005328*01  FILLER -COPY W461RIYN        -L.                                     
005329     SKIP2                                                                
005330*01  FILLER -COPY W461RIZN        -L.                                     
005331     SKIP2                                                                
005332*01  FILLER -COPY W461RKAN        -L.                                     
005333     SKIP2                                                                
005334*01  FILLER -COPY W461RKBN        -L.                                     
005335     SKIP2                                                                
005336*01  FILLER -COPY W461RKCN        -L.                                     
005337     SKIP2                                                                
005338*01  FILLER -COPY W461RKDN        -L.                                     
005339     SKIP2                                                                
005340*01  FILLER -COPY W461RKEN        -L.                                     
005341     SKIP2                                                                
005342*01  FILLER -COPY W461RKFN        -L.                                     
005343     SKIP2                                                                
005344*01  FILLER -COPY W461RKGN        -L.                                     
005345     SKIP2                                                                
005346*01  FILLER -COPY W461RKHN        -L.                                     
005347     SKIP2                                                                
005348*01  FILLER -COPY W461RKIN        -L.                                     
005349     EJECT                                                                
005350 FD  W46198                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS 0.                                                    
005600 01  UTPOST                       PIC X(80).                              
005700     SKIP2                                                                
005800 FD  W4614V                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS 0.                                                    
006100 01  UTPOST2                      PIC X(80).                              
006200     SKIP2                                                                
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006410                                                                          
006500*    -- CHECKED BY WY2000                                                 
007000*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007100 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4617800'.            
007300     SKIP2                                                                
007400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007500                                                                          
007600 77  JA                          PIC X(1)    VALUE 'J'.                   
007700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007800     SKIP2                                                                
007900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
008000                                                                          
008100 77  W46178-EOF                  PIC X(1)    VALUE 'N'.                   
008200     SKIP2                                                                
008300*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008400                                                                          
008500 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008600 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008700                                                                          
008800*- - - - - - - - - - - - - -                                              
008900                                                                          
009000     EJECT                                                                
009100 01  DAGENS-DATUM.                                                        
009200   03  DAGENS-DATUM-AR           PIC 9(2).                                
009300   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009400   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009500                                                                          
009600 01  DAGENS-TID.                                                          
009700   03  DAGENS-TID-TIM            PIC 9(2).                                
009800   03  DAGENS-TID-MIN            PIC 9(2).                                
009900   03  DAGENS-TID-SEK            PIC 9(2).                                
010000                                                                          
010100 01  DYNAMISKA-SUBPROGRAM.                                                
010200   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010300   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010500     SKIP3                                                                
010600*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010700                                                                          
010800 01  RETURKODER.                                                          
010900   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
011000   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
011100   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011200     EJECT                                                                
011300*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011400                                                                          
011500******************************************************************        
011600                                                                          
011700 01  W-ARBAREA.                                                           
011800 03  W-ARBAREA-X                 PIC X(211).                              
011900     SKIP2                                                                
012000*                                                                         
012010*03  FILLER  -COPY W461RIAN        -PRE W- -RED W-ARBAREA-X               
012020     EJECT                                                                
012030*03  FILLER  -COPY W461RIBN        -PRE W- -RED W-ARBAREA-X               
012040     EJECT                                                                
012050*03  FILLER  -COPY W461RICN        -PRE W- -RED W-ARBAREA-X               
012060     EJECT                                                                
012070*03  FILLER  -COPY W461RIDN        -PRE W- -RED W-ARBAREA-X               
012080     EJECT                                                                
012090*03  FILLER  -COPY W461RIEN        -PRE W- -RED W-ARBAREA-X               
012091     EJECT                                                                
012092*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
012093     EJECT                                                                
012094*03  FILLER  -COPY W461RIGN        -PRE W- -RED W-ARBAREA-X               
012095     EJECT                                                                
012096*03  FILLER  -COPY W461RIHN        -PRE W- -RED W-ARBAREA-X               
012097     EJECT                                                                
012098*03  FILLER  -COPY W461RIIN        -PRE W- -RED W-ARBAREA-X               
012099     EJECT                                                                
012100*03  FILLER  -COPY W461RIJN        -PRE W- -RED W-ARBAREA-X               
012110     EJECT                                                                
012120*03  FILLER  -COPY W461RIK1        -PRE W- -RED W-ARBAREA-X               
012130     EJECT                                                                
012140*03  FILLER  -COPY W461RILN        -PRE W- -RED W-ARBAREA-X               
012150     EJECT                                                                
012160*03  FILLER  -COPY W461RIM2        -PRE W- -RED W-ARBAREA-X               
012170     EJECT                                                                
012180*03  FILLER  -COPY W461RINN        -PRE W- -RED W-ARBAREA-X               
012190     EJECT                                                                
012191*03  FILLER  -COPY W461RIO2        -PRE W- -RED W-ARBAREA-X               
012192     EJECT                                                                
012193*03  FILLER  -COPY W461RIPN        -PRE W- -RED W-ARBAREA-X               
012194     EJECT                                                                
012195*03  FILLER  -COPY W461RIQN        -PRE W- -RED W-ARBAREA-X               
012196     EJECT                                                                
012197*03  FILLER  -COPY W461RIRN        -PRE W- -RED W-ARBAREA-X               
012198     EJECT                                                                
012199*03  FILLER  -COPY W461RISN        -PRE W- -RED W-ARBAREA-X               
012200     EJECT                                                                
012201*03  FILLER  -COPY W461RITN        -PRE W- -RED W-ARBAREA-X               
012202     EJECT                                                                
012203*03  FILLER  -COPY W461RIUN        -PRE W- -RED W-ARBAREA-X               
012204     EJECT                                                                
012205*03  FILLER  -COPY W461RIWN        -PRE W- -RED W-ARBAREA-X               
012206     EJECT                                                                
012207*03  FILLER  -COPY W461RIXN        -PRE W- -RED W-ARBAREA-X               
012208     EJECT                                                                
012209*03  FILLER  -COPY W461RIYN        -PRE W- -RED W-ARBAREA-X               
012210     EJECT                                                                
012211*03  FILLER  -COPY W461RIZN        -PRE W- -RED W-ARBAREA-X               
012212     EJECT                                                                
012213*03  FILLER  -COPY W461RKAN        -PRE W- -RED W-ARBAREA-X               
012214     EJECT                                                                
012215*03  FILLER  -COPY W461RKBN        -PRE W- -RED W-ARBAREA-X               
012216     EJECT                                                                
012217*03  FILLER  -COPY W461RKCN        -PRE W- -RED W-ARBAREA-X               
012218     EJECT                                                                
012219*03  FILLER  -COPY W461RKDN        -PRE W- -RED W-ARBAREA-X               
012220     EJECT                                                                
012221*03  FILLER  -COPY W461RKEN        -PRE W- -RED W-ARBAREA-X               
012222     EJECT                                                                
012223*03  FILLER  -COPY W461RKFN        -PRE W- -RED W-ARBAREA-X               
012224     EJECT                                                                
012225*03  FILLER  -COPY W461RKGN        -PRE W- -RED W-ARBAREA-X               
012226     EJECT                                                                
012227*03  FILLER  -COPY W461RKHN        -PRE W- -RED W-ARBAREA-X               
012228     EJECT                                                                
012229*03  FILLER  -COPY W461RKIN        -PRE W- -RED W-ARBAREA-X               
012230     EJECT                                                                
012231                                                                          
012232********************* GAMLA UTSEENDET ****************************        
012233**                                                                        
012234 01  W-OLDAREA.                                                           
012235 03  W-OLDAREA-X                 PIC X(80).                               
012236     SKIP2                                                                
012237*                                                                         
012238*03  FILLER  -COPY W461RIA0        -PRE O- -RED W-OLDAREA-X               
012239     EJECT                                                                
012240*03  FILLER  -COPY W461RIB0        -PRE O- -RED W-OLDAREA-X               
012241     EJECT                                                                
012242*03  FILLER  -COPY W461RIC0        -PRE O- -RED W-OLDAREA-X               
012243     EJECT                                                                
012244*03  FILLER  -COPY W461RID0        -PRE O- -RED W-OLDAREA-X               
012245     EJECT                                                                
012246*03  FILLER  -COPY W461RIE0        -PRE O- -RED W-OLDAREA-X               
012247     EJECT                                                                
012248*03  FILLER  -COPY W461RIF0        -PRE O- -RED W-OLDAREA-X               
012249     EJECT                                                                
012250*03  FILLER  -COPY W461RIG0        -PRE O- -RED W-OLDAREA-X               
012251     EJECT                                                                
012252*03  FILLER  -COPY W461RIH0        -PRE O- -RED W-OLDAREA-X               
012253     EJECT                                                                
012254*03  FILLER  -COPY W461RII0        -PRE O- -RED W-OLDAREA-X               
012255     EJECT                                                                
012256*03  FILLER  -COPY W461RIJ0        -PRE O- -RED W-OLDAREA-X               
012257     EJECT                                                                
012258*03  FILLER  -COPY W461RIK0        -PRE O- -RED W-OLDAREA-X               
012259     EJECT                                                                
012260*03  FILLER  -COPY W461RIL0        -PRE O- -RED W-OLDAREA-X               
012261     EJECT                                                                
012262*03  FILLER  -COPY W461RIMN        -PRE O- -RED W-OLDAREA-X               
012263     EJECT                                                                
012264*03  FILLER  -COPY W461RIN0        -PRE O- -RED W-OLDAREA-X               
012265     EJECT                                                                
012266*03  FILLER  -COPY W461RIO0        -PRE O- -RED W-OLDAREA-X               
012267     EJECT                                                                
012268*03  FILLER  -COPY W461RIP0        -PRE O- -RED W-OLDAREA-X               
012269     EJECT                                                                
012270*03  FILLER  -COPY W461RIQ0        -PRE O- -RED W-OLDAREA-X               
012271     EJECT                                                                
012272*03  FILLER  -COPY W461RIR0        -PRE O- -RED W-OLDAREA-X               
012273     EJECT                                                                
012274*03  FILLER  -COPY W461RIS0        -PRE O- -RED W-OLDAREA-X               
012275     EJECT                                                                
012276*03  FILLER  -COPY W461RIT0        -PRE O- -RED W-OLDAREA-X               
012277     EJECT                                                                
012278*03  FILLER  -COPY W461RIU0        -PRE O- -RED W-OLDAREA-X               
012279     EJECT                                                                
012280*03  FILLER  -COPY W461RIW0        -PRE O- -RED W-OLDAREA-X               
012281     EJECT                                                                
012282*03  FILLER  -COPY W461RIX0        -PRE O- -RED W-OLDAREA-X               
012283     EJECT                                                                
012284*03  FILLER  -COPY W461RIY0        -PRE O- -RED W-OLDAREA-X               
012285     EJECT                                                                
012286*03  FILLER  -COPY W461RIZ0        -PRE O- -RED W-OLDAREA-X               
012287     EJECT                                                                
012288*03  FILLER  -COPY W461RKA0        -PRE O- -RED W-OLDAREA-X               
012289     EJECT                                                                
012290*03  FILLER  -COPY W461RKB0        -PRE O- -RED W-OLDAREA-X               
012291     EJECT                                                                
012292*03  FILLER  -COPY W461RKC0        -PRE O- -RED W-OLDAREA-X               
012293     EJECT                                                                
012294*03  FILLER  -COPY W461RKD0        -PRE O- -RED W-OLDAREA-X               
012295     EJECT                                                                
012296*03  FILLER  -COPY W461RKE0        -PRE O- -RED W-OLDAREA-X               
012297     EJECT                                                                
012298*03  FILLER  -COPY W461RKF0        -PRE O- -RED W-OLDAREA-X               
012299     EJECT                                                                
012300*03  FILLER  -COPY W461RKG0        -PRE O- -RED W-OLDAREA-X               
012301     EJECT                                                                
012302*03  FILLER  -COPY W461RKH0        -PRE O- -RED W-OLDAREA-X               
012303     EJECT                                                                
012304*03  FILLER  -COPY W461RKI0        -PRE O- -RED W-OLDAREA-X               
012305     EJECT                                                                
012306                                                                          
012307*01  -COPY W461RIFN        -PRE JFR-.                                     
012310     EJECT                                                                
012400*                             STARTKORT                                   
012500*01  -COPY W461RI0                                                        
012700     EJECT                                                                
012800*                             SLUTKORT                                    
012900*01  -COPY W461RI9                                                        
013100     EJECT                                                                
013200*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013300                                                                          
013400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013500     SKIP2                                                                
013600*01  -COPY WDATKORT                                                       
013800     EJECT                                                                
013900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
014000                                                                          
014100*01  -COPY W0005       -PRE  POSTSUM-.                                    
014300     EJECT                                                                
014400 PROCEDURE DIVISION.                                                      
014500     SKIP2                                                                
014600     PERFORM A-INIT                                                       
014700     PERFORM B-BEHANDLA                                                   
014800     PERFORM Z-FINIT                                                      
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200                                                                          
015300     SKIP3                                                                
015400 A-INIT SECTION.                                                          
015500     SKIP2                                                                
015600     OPEN INPUT W46178 OUTPUT W46198 W4614V                               
015700     SKIP2                                                                
015800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015900     SKIP2                                                                
016000*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
016100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016200     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016300     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016400     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016500     SKIP2                                                                
016600*- - - - - - - - - - - - - - - - TID                                      
016700*                                                                         
016800     ACCEPT   DAGENS-TID FROM TIME                                        
016900     .                                                                    
017000     EJECT                                                                
017100                                                                          
017200 B-BEHANDLA SECTION.                                                      
017300     SKIP2                                                                
017400     PERFORM BA-START-KORT                                                
017500     PERFORM S01-LAS-W46178                                               
017600     PERFORM UNTIL                                                        
017700      NOT ( W46178-EOF = NEJ )                                            
017800       IF W-RIF-IDPTYP = 'RIA' OR 'RIB' OR 'RID' OR 'RIE' OR              
017900         'RIF' OR 'RIG' OR 'RIH' OR 'RII' OR 'RIK' OR 'RIL' OR            
018000         'RIM' OR 'RIN' OR 'RIO' OR 'RIP' OR 'RIT'                        
018010         EVALUATE      W-RIA-IDPTYP                                       
018020                                                                          
018030           WHEN    'RIA'                                                  
018040                PERFORM B-RIA-AENDRA-TILL-OLD                             
018050                                                                          
018060           WHEN    'RIB'                                                  
018070                PERFORM B-RIB-AENDRA-TILL-OLD                             
018080                                                                          
018090           WHEN    'RIC'                                                  
018091                PERFORM B-RIC-AENDRA-TILL-OLD                             
018092                                                                          
018093           WHEN    'RID'                                                  
018094                PERFORM B-RID-AENDRA-TILL-OLD                             
018095                                                                          
018096           WHEN    'RIE'                                                  
018097                PERFORM B-RIE-AENDRA-TILL-OLD                             
018098                                                                          
018099           WHEN    'RIF'                                                  
018100                PERFORM B-RIF-AENDRA-TILL-OLD                             
018101                                                                          
018102           WHEN    'RIG'                                                  
018103                PERFORM B-RIG-AENDRA-TILL-OLD                             
018104                                                                          
018105           WHEN    'RIH'                                                  
018106                PERFORM B-RIH-AENDRA-TILL-OLD                             
018107                                                                          
018108           WHEN    'RII'                                                  
018109                PERFORM B-RII-AENDRA-TILL-OLD                             
018110                                                                          
018111           WHEN    'RIJ'                                                  
018112                PERFORM B-RIJ-AENDRA-TILL-OLD                             
018113                                                                          
018114           WHEN    'RIK'                                                  
018115                PERFORM B-RIK-AENDRA-TILL-OLD                             
018116                                                                          
018117           WHEN    'RIL'                                                  
018118                PERFORM B-RIL-AENDRA-TILL-OLD                             
018119                                                                          
018120           WHEN    'RIM'                                                  
018121                PERFORM C-SKRIV-RIMPOST-GAMMAL                            
018122                                                                          
018123           WHEN    'RIN'                                                  
018124                PERFORM B-RIN-AENDRA-TILL-OLD                             
018125                                                                          
018126           WHEN    'RIO'                                                  
018127                PERFORM B-RIO-AENDRA-TILL-OLD                             
018128                                                                          
018129           WHEN    'RIP'                                                  
018130                PERFORM B-RIP-AENDRA-TILL-OLD                             
018131                                                                          
018132           WHEN    'RIQ'                                                  
018133                PERFORM B-RIQ-AENDRA-TILL-OLD                             
018134                                                                          
018135           WHEN    'RIR'                                                  
018136                PERFORM B-RIR-AENDRA-TILL-OLD                             
018137                                                                          
018141           WHEN    'RIT'                                                  
018142                PERFORM B-RIT-AENDRA-TILL-OLD                             
018170                                                                          
018186         END-EVALUATE                                                     
018187         ADD     +1           TO W-ANT-POSTER                             
018188         WRITE   UTPOST   FROM W-OLDAREA                                  
018189         WRITE   UTPOST2  FROM W-OLDAREA                                  
018190         PERFORM S01-LAS-W46178                                           
018191       ELSE                                                               
018192         PERFORM S01-LAS-W46178                                           
018193       END-IF                                                             
018194     END-PERFORM                                                          
018195     PERFORM BB-SLUT-KORT                                                 
018196     .                                                                    
018197     EJECT                                                                
018198                                                                          
018199 B-RIA-AENDRA-TILL-OLD SECTION.                                           
018200                                                                          
018201     MOVE W-RIA-IDPTYP       TO O-RIA-IDPTYP                              
018202     MOVE W-RIA-IDDC(1:1)    TO O-RIA-KDCLAGER                            
018203     MOVE W-RIA-IDDISTR      TO O-RIA-IDDISTR                             
018204     MOVE W-RIA-IDKUNDNR     TO O-RIA-IDKUNDNR                            
018205     MOVE W-RIA-IDORDNR      TO O-RIA-IDORDNR                             
018206     MOVE W-RIA-KDORDKL      TO O-RIA-KDORDKL                             
018207     MOVE W-RIA-IDARTNR      TO O-RIA-IDARTNR                             
018208     MOVE W-RIA-REKSIFFR     TO O-RIA-REKSIFFR                            
018209     MOVE W-RIA-BERADREF     TO O-RIA-BERADREF                            
018210     MOVE W-RIA-IDRONR       TO O-RIA-IDRONR                              
018211     MOVE W-RIA-BEVOLREF     TO O-RIA-BEVOLREF                            
018212     MOVE W-RIA-KVLEVART     TO O-RIA-KVLEVART                            
018213     MOVE W-RIA-KDRESTR      TO O-RIA-KDRESTR                             
018214     MOVE W-RIA-TIMM         TO O-RIA-TIMM                                
018215     MOVE W-RIA-TIDD         TO O-RIA-TIDD                                
018216     MOVE W-RIA-TIKLOCK      TO O-RIA-TIKLOCK                             
018217     .                                                                    
018218     EJECT                                                                
018219                                                                          
018220 B-RIB-AENDRA-TILL-OLD SECTION.                                           
018221                                                                          
018222     MOVE W-ARBAREA          TO W-OLDAREA                                 
018223     .                                                                    
018224     EJECT                                                                
018225                                                                          
018226 B-RIC-AENDRA-TILL-OLD SECTION.                                           
018227                                                                          
018228     MOVE W-ARBAREA          TO W-OLDAREA                                 
018229     .                                                                    
018230     EJECT                                                                
018231                                                                          
018232 B-RID-AENDRA-TILL-OLD SECTION.                                           
018233                                                                          
018234     MOVE W-RID-IDPTYP         TO O-RID-IDPTYP                            
018235     MOVE W-RID-IDDC(1:1)      TO O-RID-KDCLAGER                          
018236     MOVE W-RID-IDARTNR        TO O-RID-IDARTNR                           
018237     MOVE W-RID-REKSIFFR       TO O-RID-REKSIFFR                          
018238     MOVE W-RID-IDLOPNRE       TO O-RID-IDLOPNRE                          
018239     MOVE W-RID-IDKORTNR       TO O-RID-IDKORTNR                          
018240     MOVE W-RID-BERADREF       TO O-RID-BERADREF                          
018241     MOVE W-RID-IDRONR         TO O-RID-IDRONR                            
018242     MOVE W-RID-BEVOLREF       TO O-RID-BEVOLREF                          
018243     MOVE W-RID-KDRESTR        TO O-RID-KDRESTR                           
018244     MOVE W-RID-KDERS          TO O-RID-KDERS                             
018245     MOVE W-RID-KVBEART        TO O-RID-KVBEART                           
018246     MOVE W-RID-IDARTNR-TILLK  TO O-RID-IDARTNR-TILLK                     
018247     MOVE W-RID-REKSIFFR-TILLK TO O-RID-REKSIFFR-TILLK                    
018248     MOVE W-RID-KVBEART-TILLK  TO O-RID-KVBEART-TILLK                     
018249     MOVE W-RID-DIERS-KVOT     TO O-RID-DIERS-KVOT                        
018250     MOVE W-RID-KDERSUP        TO O-RID-KDERSUP                           
018251     MOVE W-RID-KDDSP          TO O-RID-KDDSP                             
018252     .                                                                    
018253     EJECT                                                                
018254                                                                          
018255 B-RIE-AENDRA-TILL-OLD SECTION.                                           
018256                                                                          
018257     MOVE W-RIE-IDPTYP         TO O-RIE-IDPTYP                            
018258     MOVE W-RIE-IDDC(1:1)      TO O-RIE-KDCLAGER                          
018259     MOVE W-RIE-IDARTNR        TO O-RIE-IDARTNR                           
018260     MOVE W-RIE-REKSIFFR       TO O-RIE-REKSIFFR                          
018261     MOVE W-RIE-IDLOPNRE       TO O-RIE-IDLOPNRE                          
018262     MOVE W-RIE-IDKORTNR       TO O-RIE-IDKORTNR                          
018263     MOVE W-RIE-BERADREF       TO O-RIE-BERADREF                          
018264     MOVE W-RIE-IDRONR         TO O-RIE-IDRONR                            
018265     MOVE W-RIE-BEVOLREF       TO O-RIE-BEVOLREF                          
018266     MOVE W-RIE-KDRESTR        TO O-RIE-KDRESTR                           
018267     MOVE W-RIE-KDERS          TO O-RIE-KDERS                             
018268     MOVE W-RIE-KVBEART        TO O-RIE-KVBEART                           
018269     MOVE W-RIE-IDARTNR-TILLK  TO O-RIE-IDARTNR-TILLK                     
018270     MOVE W-RIE-REKSIFFR-TILLK TO O-RIE-REKSIFFR-TILLK                    
018271     MOVE W-RIE-KVBEART-TILLK  TO O-RIE-KVBEART-TILLK                     
018272     MOVE W-RIE-DIERS-KVOT     TO O-RIE-DIERS-KVOT                        
018273     MOVE W-RIE-KDERSUP        TO O-RIE-KDERSUP                           
018274     MOVE W-RIE-KDDSP          TO O-RIE-KDDSP                             
018275     .                                                                    
018276     EJECT                                                                
018277                                                                          
018278 B-RIF-AENDRA-TILL-OLD SECTION.                                           
018279                                                                          
018280     MOVE W-RIF-IDPTYP         TO O-RIF-IDPTYP                            
018281     MOVE W-RIF-IDDC(1:1)      TO O-RIF-KDCLAGER                          
018282     MOVE W-RIF-IDARTNR        TO O-RIF-IDARTNR                           
018283     MOVE W-RIF-REKSIFFR       TO O-RIF-REKSIFFR                          
018284     MOVE W-RIF-IDLOPNRE       TO O-RIF-IDLOPNRE                          
018285     MOVE W-RIF-IDKORTNR       TO O-RIF-IDKORTNR                          
018286     MOVE W-RIF-BERADREF       TO O-RIF-BERADREF                          
018287     MOVE W-RIF-IDRONR         TO O-RIF-IDRONR                            
018288     MOVE W-RIF-BEVOLREF       TO O-RIF-BEVOLREF                          
018289     MOVE W-RIF-KDRESTR        TO O-RIF-KDRESTR                           
018290     MOVE W-RIF-KDERS          TO O-RIF-KDERS                             
018291     MOVE W-RIF-KVBEART        TO O-RIF-KVBEART                           
018292     MOVE W-RIF-BEERS          TO O-RIF-BEERS                             
018293     MOVE W-RIF-KDERSUP        TO O-RIF-KDERSUP                           
018294     MOVE W-RIF-KDDSP          TO O-RIF-KDDSP                             
018295     .                                                                    
018296     EJECT                                                                
018297                                                                          
018298 B-RIG-AENDRA-TILL-OLD SECTION.                                           
018299                                                                          
018300     MOVE W-RIG-IDPTYP         TO O-RIG-IDPTYP                            
018301     MOVE W-RIG-IDDC(1:1)      TO O-RIG-KDCLAGER                          
018302     MOVE W-RIG-IDARTNR        TO O-RIG-IDARTNR                           
018303     MOVE W-RIG-REKSIFFR       TO O-RIG-REKSIFFR                          
018304     MOVE W-RIG-BERADREF       TO O-RIG-BERADREF                          
018305     MOVE W-RIG-IDRONR         TO O-RIG-IDRONR                            
018306     MOVE W-RIG-BEVOLREF       TO O-RIG-BEVOLREF                          
018307     MOVE W-RIG-KDRESTR        TO O-RIG-KDRESTR                           
018308     MOVE W-RIG-KVBEART        TO O-RIG-KVBEART                           
018309     MOVE W-RIG-KVBEART-Q      TO O-RIG-KVBEART-Q                         
018310     MOVE W-RIG-KVQPACK-1      TO O-RIG-KVQPACK-1                         
018311     MOVE W-RIG-KDDSP          TO O-RIG-KDDSP                             
018312     MOVE W-RIG-TIMM           TO O-RIG-TIMM                              
018313     MOVE W-RIG-TIDD           TO O-RIG-TIDD                              
018314     MOVE W-RIG-TIKLOCK        TO O-RIG-TIKLOCK                           
018315     .                                                                    
018316     EJECT                                                                
018317                                                                          
018318 B-RIH-AENDRA-TILL-OLD SECTION.                                           
018319                                                                          
018320     MOVE W-RIH-IDPTYP         TO O-RIH-IDPTYP                            
018321     MOVE W-RIH-IDDC(1:1)      TO O-RIH-KDCLAGER                          
018322     MOVE W-RIH-IDARTNR        TO O-RIH-IDARTNR                           
018323     MOVE W-RIH-REKSIFFR       TO O-RIH-REKSIFFR                          
018324     MOVE W-RIH-BERADREF       TO O-RIH-BERADREF                          
018325     MOVE W-RIH-IDRONR         TO O-RIH-IDRONR                            
018326     MOVE W-RIH-BEVOLREF       TO O-RIH-BEVOLREF                          
018327     MOVE W-RIH-KDRESTR        TO O-RIH-KDRESTR                           
018328     MOVE W-RIH-KVBEART        TO O-RIH-KVBEART                           
018329     MOVE W-RIH-KVAVBART       TO O-RIH-KVAVBART                          
018330     MOVE W-RIH-KVRO           TO O-RIH-KVRO                              
018331     MOVE W-RIH-KDDSP          TO O-RIH-KDDSP                             
018332     MOVE W-RIH-TIDISPIN       TO O-RIH-TIDISPIN                          
018333     MOVE W-RIH-TIMM           TO O-RIH-TIMM                              
018334     MOVE W-RIH-TIDD           TO O-RIH-TIDD                              
018335     MOVE W-RIH-TIKLOCK        TO O-RIH-TIKLOCK                           
018336     .                                                                    
018337     EJECT                                                                
018338                                                                          
018339 B-RII-AENDRA-TILL-OLD SECTION.                                           
018340                                                                          
018341     MOVE W-RII-IDPTYP         TO O-RII-IDPTYP                            
018342     MOVE W-RII-IDDC(1:1)      TO O-RII-KDCLAGER                          
018343     MOVE W-RII-IDARTNR        TO O-RII-IDARTNR                           
018344     MOVE W-RII-REKSIFFR       TO O-RII-REKSIFFR                          
018345     MOVE W-RII-BERADREF       TO O-RII-BERADREF                          
018346     MOVE W-RII-IDRONR         TO O-RII-IDRONR                            
018347     MOVE W-RII-BEVOLREF       TO O-RII-BEVOLREF                          
018348     MOVE W-RII-KDRESTR        TO O-RII-KDRESTR                           
018349     MOVE W-RII-KVBEART        TO O-RII-KVBEART                           
018350     MOVE W-RII-KDDSP          TO O-RII-KDDSP                             
018351     MOVE W-RII-TIMM           TO O-RII-TIMM                              
018352     MOVE W-RII-TIDD           TO O-RII-TIDD                              
018353     MOVE W-RII-TIKLOCK        TO O-RII-TIKLOCK                           
018354     .                                                                    
018355     EJECT                                                                
018356                                                                          
018357 B-RIJ-AENDRA-TILL-OLD SECTION.                                           
018358                                                                          
018359     MOVE W-RIJ-IDPTYP         TO O-RIJ-IDPTYP                            
018360     MOVE W-RIJ-IDDC(1:1)      TO O-RIJ-KDCLAGER                          
018361     MOVE W-RIJ-IDDISTR        TO O-RIJ-IDDISTR                           
018362     MOVE W-RIJ-IDKUNDNR       TO O-RIJ-IDKUNDNR                          
018363     MOVE W-RIJ-IDORDNR        TO O-RIJ-IDORDNR                           
018364     MOVE W-RIJ-KDORDKL        TO O-RIJ-KDORDKL                           
018365     MOVE W-RIJ-IDARTNR        TO O-RIJ-IDARTNR                           
018366     MOVE W-RIJ-REKSIFFR       TO O-RIJ-REKSIFFR                          
018367     MOVE W-RIJ-KVBEART        TO O-RIJ-KVBEART                           
018368     .                                                                    
018369     EJECT                                                                
018370                                                                          
018371 B-RIK-AENDRA-TILL-OLD SECTION.                                           
018372                                                                          
018373     MOVE W-RIK-IDPTYP         TO O-RIK-IDPTYP                            
018374     MOVE W-RIK-IDDC(1:1)      TO O-RIK-KDCLAGER                          
018375     MOVE W-RIK-IDDISTR        TO O-RIK-IDDISTR                           
018376     MOVE W-RIK-KDFAKTYP       TO O-RIK-KDFAKTYP                          
018377     MOVE W-RIK-IDFAKT         TO O-RIK-IDFAKT                            
018378     MOVE W-RIK-TIFAKT         TO O-RIK-TIFAKT                            
018379     MOVE W-RIK-IDFRASED       TO O-RIK-IDFRASED                          
018380     MOVE W-RIK-SUFKTBEL       TO O-RIK-SUFKTBEL                          
018381     MOVE W-RIK-KDVALUTA       TO O-RIK-KDVALUTA                          
018382     MOVE W-RIK-PRKURS         TO O-RIK-PRKURS                            
018383     MOVE W-RIK-SUFKTUTL       TO O-RIK-SUFKTUTL                          
018384     MOVE W-RIK-KDFAKNOT       TO O-RIK-KDFAKNOT                          
018385     .                                                                    
018386     EJECT                                                                
018387                                                                          
018388 B-RIL-AENDRA-TILL-OLD SECTION.                                           
018389                                                                          
018390     MOVE W-ARBAREA            TO W-OLDAREA                               
018391     .                                                                    
018392     EJECT                                                                
018393                                                                          
018394 B-RIM-AENDRA-TILL-OLD SECTION.                                           
018395                                                                          
018396     MOVE W-RIM-IDPTYP         TO O-RIM-IDPTYP                            
018397     MOVE W-RIM-IDKUNDNR       TO O-RIM-IDKUNDNR                          
018398     MOVE W-RIM-IDORDNR        TO O-RIM-IDORDNR                           
018399     MOVE W-RIM-IDPRODNR       TO O-RIM-IDPRODNR                          
018400     MOVE W-RIM-TIORDREG       TO O-RIM-TIORDREG                          
018401     MOVE W-RIM-KDORDKL        TO O-RIM-KDORDKL                           
018402     MOVE W-RIM-BEKUNDRF       TO O-RIM-BEVOLREF                          
018403     MOVE W-RIM-BEVARREF       TO O-RIM-BEVARREF                          
018404     MOVE W-RIM-KDREFNOT       TO O-RIM-KDREFNOT                          
018405     MOVE W-RIM-KDFRAKT        TO O-RIM-KDFRAKT                           
018406     MOVE W-RIM-IDTRPBOT       TO O-RIM-IDTRPBOT                          
018407     MOVE W-RIM-IDTRPBON       TO O-RIM-IDTRPBON                          
018408     .                                                                    
018409     EJECT                                                                
018410                                                                          
018411 B-RIN-AENDRA-TILL-OLD SECTION.                                           
018412                                                                          
018413     MOVE W-ARBAREA            TO W-OLDAREA                               
018414     .                                                                    
018415     EJECT                                                                
018416                                                                          
018417 B-RIO-AENDRA-TILL-OLD SECTION.                                           
018418                                                                          
018419     MOVE W-RIO-IDPTYP         TO O-RIO-IDPTYP                            
018420     MOVE W-RIO-IDORDNR        TO O-RIO-IDORDNR                           
018421     MOVE W-RIO-IDARTNR        TO O-RIO-IDARTNR                           
018422     MOVE W-RIO-REKSIFFR       TO O-RIO-REKSIFFR                          
018423     MOVE W-RIO-BERADREF       TO O-RIO-BERADREF                          
018424     MOVE W-RIO-KVBEART        TO O-RIO-KVBEART                           
018425     MOVE W-RIO-KVLEVART       TO O-RIO-KVLEVART                          
018426     MOVE W-RIO-RESERVG        TO O-RIO-RESERVG                           
018427     MOVE W-RIO-PRARTBTO-EXP   TO O-RIO-PRARTBTO-EXP                      
018428     MOVE W-RIO-PRARTNTO       TO O-RIO-PRARTNTO                          
018429     MOVE W-RIO-IDFKNGRP       TO O-RIO-IDFKNGRP                          
018430     MOVE W-RIO-KDPRODSL       TO O-RIO-KDPRODSL                          
018431     MOVE W-RIO-KDDSP          TO O-RIO-KDDSP                             
018432     MOVE W-RIO-KDVVKL         TO O-RIO-KDVVKL                            
018433     MOVE W-RIO-KDVRINFO       TO O-RIO-KDVRINFO                          
018434     MOVE W-RIO-FLINVEST       TO O-RIO-FLINVEST                          
018435     MOVE W-RIO-FLPRTILL       TO O-RIO-FLPRTILL                          
018436     MOVE W-RIO-FLDIRLEV       TO O-RIO-FLDIRLEV                          
018437     MOVE W-RIO-KDRABATT       TO O-RIO-KDRABATT                          
018438     .                                                                    
018439     EJECT                                                                
018440                                                                          
018441 B-RIP-AENDRA-TILL-OLD SECTION.                                           
018442                                                                          
018443     MOVE W-ARBAREA            TO W-OLDAREA                               
018444     .                                                                    
018445     EJECT                                                                
018446                                                                          
018447 B-RIQ-AENDRA-TILL-OLD SECTION.                                           
018448                                                                          
018449     MOVE W-RIQ-IDPTYP         TO O-RIQ-IDPTYP                            
018450     MOVE W-RIQ-IDDISTR        TO O-RIQ-IDDISTR                           
018451     MOVE W-RIQ-IDKUNDNR       TO O-RIQ-IDKUNDNR                          
018452     MOVE W-RIQ-IDORDNR        TO O-RIQ-IDORDNR                           
018453     MOVE W-RIQ-IDDC(1:1)      TO O-RIQ-KDCLAGER                          
018454     MOVE W-RIQ-KDFAKTYP       TO O-RIQ-KDFAKTYP                          
018455     MOVE W-RIQ-IDFAKT         TO O-RIQ-IDFAKT                            
018456     MOVE W-RIQ-TIFAKT         TO O-RIQ-TIFAKT                            
018457     MOVE W-RIQ-KDPALL         TO O-RIQ-KDPALL                            
018458     MOVE W-RIQ-KVPALL         TO O-RIQ-KVPALL                            
018459     MOVE W-RIQ-KVKRAG         TO O-RIQ-KVKRAG                            
018460     MOVE W-RIQ-KVLOCK         TO O-RIQ-KVLOCK                            
018461     .                                                                    
018462     EJECT                                                                
018463                                                                          
018464 B-RIR-AENDRA-TILL-OLD SECTION.                                           
018465                                                                          
018466     MOVE W-RIR-IDPTYP         TO O-RIR-IDPTYP                            
018467     MOVE W-RIR-IDDC(1:1)      TO O-RIR-KDCLAGER                          
018468     MOVE W-RIR-IDDISTR        TO O-RIR-IDDISTR                           
018469     MOVE W-RIR-IDKUNDNR       TO O-RIR-IDKUNDNR                          
018470     MOVE W-RIR-IDORDNR        TO O-RIR-IDORDNR                           
018471     MOVE W-RIR-BEVOLREF       TO O-RIR-BEVOLREF                          
018472     MOVE W-RIR-TIORDREG       TO O-RIR-TIORDREG                          
018473     .                                                                    
018474     EJECT                                                                
018475                                                                          
018501                                                                          
018502 B-RIT-AENDRA-TILL-OLD SECTION.                                           
018503                                                                          
018504     MOVE W-ARBAREA            TO W-OLDAREA                               
018505     .                                                                    
018506     EJECT                                                                
018560                                                                          
019200 BA-START-KORT SECTION.                                                   
019300     SKIP2                                                                
019400     MOVE     'RI0'          TO START-IDPTYP                              
019500     MOVE     4840           TO START-IDDISTR                             
019600     MOVE     1              TO START-KDCLAGER                            
019700     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
019800*                                                                         
019900     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020000*                                                                         
020100     DISPLAY  'START-KORT  ' START-W461RI0                                
020200     WRITE    UTPOST         FROM  START-W461RI0                          
020300     WRITE    UTPOST2        FROM  START-W461RI0                          
020400     .                                                                    
020500     SKIP2                                                                
020600 BB-SLUT-KORT SECTION.                                                    
020700     SKIP2                                                                
020800     MOVE     'RI9'          TO SLUT-IDPTYP                               
020900     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021000*                                                                         
021100     WRITE    UTPOST         FROM  SLUT-W461RI9                           
021200     WRITE    UTPOST2        FROM  SLUT-W461RI9                           
021300     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
021400     .                                                                    
021500     SKIP2                                                                
021510 C-SKRIV-RIMPOST-GAMMAL SECTION.                                          
021520                                                                          
021530     MOVE W-RIM-IDPTYP         TO O-RIM-IDPTYP                            
021540     MOVE W-RIM-IDKUNDNR       TO O-RIM-IDKUNDNR                          
021550     MOVE W-RIM-IDORDNR        TO O-RIM-IDORDNR                           
021560     MOVE W-RIM-IDPRODNR       TO O-RIM-IDPRODNR                          
021570     MOVE W-RIM-TIORDREG       TO O-RIM-TIORDREG                          
021580     MOVE W-RIM-KDORDKL        TO O-RIM-KDORDKL                           
021590     MOVE W-RIM-BEKUNDRF       TO O-RIM-BEVOLREF                          
021591     MOVE W-RIM-BEVARREF       TO O-RIM-BEVARREF                          
021592     MOVE W-RIM-KDREFNOT       TO O-RIM-KDREFNOT                          
021593     MOVE W-RIM-KDFRAKT        TO O-RIM-KDFRAKT                           
021594     MOVE W-RIM-IDTRPBOT       TO O-RIM-IDTRPBOT                          
021595     MOVE W-RIM-IDTRPBON       TO O-RIM-IDTRPBON                          
021596     .                                                                    
021597     EJECT                                                                
021600 S01-LAS-W46178 SECTION.                                                  
021700     SKIP2                                                                
021800     READ   W46178 INTO W-ARBAREA                                         
021900     AT END MOVE JA TO W46178-EOF                                         
022000     END-READ                                                             
022100                                                                          
022200     IF W46178-EOF = NEJ                                                  
022300                                                                          
022400       MOVE 'W46178'            TO POSTSUM-FDNAMN                         
022500       MOVE 'W46178D1'          TO POSTSUM-DDNAMN2                        
022600       MOVE SPACE               TO POSTSUM-TRANSTYP                       
022700       CALL POSTSUM             USING POSTSUM-PARM                        
022800                                                                          
022900     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 Z-FINIT SECTION.                                                         
023300     SKIP2                                                                
023400                                                                          
023500     CLOSE W46178 W46198 W4614V                                           
023600     SKIP2                                                                
023700*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023800*                                    SKRIVNA POSTER                       
023900                                                                          
024000     MOVE 'S' TO POSTSUM-OPKOD                                            
024100     CALL POSTSUM USING POSTSUM-PARM                                      
024200     .                                                                    
