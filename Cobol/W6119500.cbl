000100 ID DIVISION.                                                             
000400 PROGRAM-ID.     W6119500.                                                
000600 AUTHOR.         SVEN-OLOF JOHANSSON.                                     
000700 DATE-WRITTEN.   MAJ 1975.                                                
000800 DATE-COMPILED.                                                           
000900*                                                                         
001100*******  F U N K T I O N  *******                                         
001200*                                                                         
001300*    PROGRAMMET TAR HAND OM DE TRANSAKTIONER FRÅN W611                    
001400*    SOM SKAPATS UNDER VECKAN.   DESSA TRANSAKTIONER SUMMERAS FÖR         
001500*    INFO TILL INKÖP PV/LV.                                               
001600*    INLEVERANSTRANS. ANVÄNDES OCKSÅ FÖR FRAMSTÄLLNING                    
001700*    AV MOTTAGNINGSRAPPORTLISTA( OCH MRLISTA-JUSTERING).                  
001800*                                                                         
001900*    PROGRAMMET ÄR EN COBOL-SORT MED  OUTPUT-PROCEDURE.                   
002000     SKIP2                                                                
002100********  I N D A T A  ********                                           
002200*                                                                         
002300*  ********* *****************  ********  ************** *********        
002400*  *DD-NAMN  INNEHÅLL I FIL **  *FILNAMN  *POSTTYP****** COPYTEXT*        
002500*  ********* *****************  ********  ************** *********        
002600*                                                                         
002700*   W61195D1                     W61125      310         W211310          
002800*     -"-                          -"-       320           -"-            
002900*     -"-                          -"-       330           -"-            
003000*     -"-                          -"-       340           -"-            
003100*     -"-                          -"-       400                          
003200     EJECT                                                                
003300********  U T D A T A  ********                                           
003400*                                                                         
003500*  ********* *****************  ********                                  
003600*  *DD-NAMN  INNEHÅLL I FIL **  *FILNAMN                                  
003700*  ********* *****************  ********                                  
003800*                                                                         
003900*    W61195D2  (SUMM. INTRANS)   W61195                                   
004000*    W61195D3   W61195-01        LISTA                                    
004100*     -Ä-       W61195-02        LISTA                                    
004200*    SYSOUT   FELMEDDELANDEN                                              
004300*   V16247D3 (ANVÄNDES AV POSTSUM)                                        
004400     SKIP3                                                                
004500******* ÖVRIGA COPYTEXTER *******                                         
004600*    W0005   FÖR POSTSUM                                                  
004700     SKIP3                                                                
004800*******  R E T U R K O D E R  *******                                     
004900*                                                                         
005000*    0000    NORMALT SLUT                                                 
005100*    0020    FEL I SORTERINGEN,  EJ DUMP                                  
005200     SKIP3                                                                
005300*******  S U B P R O G R A M  *******                                     
005400*                                                                         
005500*    POSTSUM   USING  PS-PARM                                             
005600*    ABEND     USING RKOD                                                 
005700*                                                                         
005800     EJECT                                                                
005900 ENVIRONMENT DIVISION.                                                    
006000     SKIP2                                                                
006100 CONFIGURATION SECTION.                                                   
006200 SPECIAL-NAMES.                                                           
006300     C01 IS NYSIDA.                                                       
006400     SKIP2                                                                
006500 INPUT-OUTPUT SECTION.                                                    
006600*                                                                         
006700 FILE-CONTROL.                                                            
006800*                                                                         
006900     SELECT INFILER   ASSIGN TO UT-S-W61195D1.                            
007000     SELECT  W61195    ASSIGN TO UT-S-W61195D2.                           
007100     SELECT  LISTORNA  ASSIGN TO UT-S-W61195D3.                           
007200     SELECT  SRTFIL    ASSIGN TO UT-S-W61195DS.                           
007300     EJECT                                                                
007400 DATA DIVISION.                                                           
007500*                                                                         
007600 FILE SECTION.                                                            
007700     SKIP2                                                                
007800 FD  INFILER                                                              
007900     LABEL RECORDS STANDARD                                               
008000     BLOCK CONTAINS 0 RECORDS                                             
008100     RECORDING F.                                                         
008200*                                                                         
008300*01  AREA    -COPY W211310 -PRE IN-                                       
008500     EJECT                                                                
008600 FD  W61195                                                               
008700     LABEL RECORDS STANDARD                                               
008800     BLOCK CONTAINS 0 RECORDS                                             
008900     RECORDING F.                                                         
009000*                                                                         
009100*01  POST    -COPY W211310 -PRE UT-                                       
009300     EJECT                                                                
009400 FD  LISTORNA                                                             
009500     LABEL RECORDS STANDARD                                               
009600     RECORDING F.                                                         
009700 01  W61195-00X-POST               PIC X(121).                            
009800*                                                                         
009900     EJECT                                                                
010000 SD  SRTFIL                                                               
010100     RECORDING MODE F.                                                    
010200*                                                                         
010300****** SORTERINGSORDNING (STIGANDE) ******                                
010400*    1.  SORTFLT1                                                         
010500*    2.  IDANSKNR                                                         
010600*    3.  KDCLAGER-S                                                       
010700*    4.  IDLEVNR-INL                                                      
010800*    5.  IDARTNR-S                                                        
010900*    6.  IDLOPNR                                                          
011000*    7.  IDPTYP                                                           
011100*                                                                         
011200*                                                                         
011300*01  POST     -COPY W211310 -PRE SRT-                                     
011500     EJECT                                                                
011600 WORKING-STORAGE SECTION.                                                 
011601                                                                          
011610*    -- CHECKED BY WY2000                                                 
011700*                                                                         
011800*                                                                         
011900 77  SW-W61195-02        PIC X           VALUE 'N'.                       
012000 77  JA                  PIC X           VALUE 'J'.                       
012100 77  NEJ                 PIC X           VALUE 'N'.                       
012200 77  FLAG-DELETE-310-320 PIC X           VALUE 'J'.                       
012300 77  EOF-SRTFIL          PIC X           VALUE 'N'.                       
012400 77  RKOD                PIC S9(4)   COMP SYNC   VALUE ZERO.              
012500*                                                                         
012600 77  RAD                 PIC S9(3)       VALUE +99 COMP-3.                
012700 77  SIDA                PIC S9(5)       VALUE +0  COMP-3.                
012800 77  BRYT                PIC X           VALUE 'J'.                       
012900 77  BRYT-START          PIC X           VALUE 'J'.                       
013000*                                                                         
013100 77  BRYT-GRP            PIC X(7)        VALUE SPACE.                     
013200 77  BRYT-SEKT           PIC X(8)        VALUE SPACE.                     
013300 77  BRYT-TOT            PIC X(8)        VALUE SPACE.                     
013400 77  BRYT-ANSK           PIC 9(3)        VALUE 0   COMP-3.                
013500 77  BRYT-CL             PIC 9(1)        VALUE 0   COMP-3.                
013600 77  BRYT-LEVNR          PIC X(5)        VALUE SPACE.                     
013700 77  BRYT-IDAFFOMR       PIC 9(3)        VALUE 0   COMP-3.                
013800 77  BRYT-TEAFFOMR       PIC X(8)        VALUE SPACE.                     
013900*                                                                         
014000 01  LISTAREA.                                                            
014100     03  FILLER          PIC X.                                           
014200     03  LISTRAD         PIC X(120).                                      
014300     EJECT                                                                
014400*                                                                         
014500 01  FAELT-FOR-LISTOR.                                                    
014600     03  W-LISTNR-RUB    PIC 9           VALUE 1.                         
014700     03  W-JUSTERING-RUB PIC X(10)       VALUE SPACES.                    
014800     03  W-DATUM         PIC 9(6).                                        
014900     03  W-IDPTYP        PIC XX.                                          
015000     03  W-ANTAL         PIC S9(6).                                       
015100     03  W-VARDE         PIC S9(8)V99   COMP-3.                           
015200     03  W-VARDE-RT7     PIC S9(8)V99   COMP-3.                           
015300     03  W-VARDE-RT9     PIC S9(8)V99   COMP-3.                           
015400     03  W-VARDE-RT9-OVR PIC S9(8)V99   COMP-3.                           
015500     03  W-TOT-VARDE     PIC S9(9)V99   COMP-3  VALUE ZERO.               
015600     03  W-TOT-RT7       PIC S9(9)V99   COMP-3  VALUE ZERO.               
015700     03  W-TOT-RT9       PIC S9(9)V99   COMP-3  VALUE ZERO.               
015800     03  W-TOT-RT9-OVR   PIC S9(9)V99   COMP-3  VALUE ZERO.               
015900     03  W-GRP-CTRL      PIC X(7).                                        
016000     03  W-SEKT-CTRL     PIC X(8).                                        
016100     03  W-TOT-CTRL      PIC X(8).                                        
016200     03  W-IDAFFOMR      PIC 9(3) VALUE ZERO.                             
016300     03  W-TEAFFOMR      PIC X(8) VALUE SPACE.                            
016400*                                                                         
016500 01  SYSOUTMEDDELANDE.                                                    
016600     03  SORTFEL         PIC X(46)   VALUE                                
016700      '  W61195: RETURKOD=0020, FEL I SORTERINGSFASEN'.                   
016800*                                                                         
016900 01  GENERELLA-SUBPROGRAM.                                                
017000     03  POSTSUM         PIC X(8)    VALUE  'POSTSUM '.                   
017100     03  ABEND           PIC X(8)    VALUE  'ABEND '.                     
017200     03  W200ANSK        PIC X(8)    VALUE  'W200ANSK'.                   
017300     EJECT                                                                
017400*01  W-AREA  -COPY  W211310                                               
017600     EJECT                                                                
017700*01  -PRE PS-  -COPY W0005                                                
017900     EJECT                                                                
018000*01  -COPY W009W42   -PRE ANSKGRP-                                        
018200     EJECT                                                                
018300 01  RUBRAD1.                                                             
018400     03  FILLER      PIC X(15)   VALUE ' RA-ANSKAFFNING'.                 
018500     03  FILLER      PIC X(4)    VALUE SPACE.                             
018600     03  FILLER      PIC X(8)    VALUE 'W61195-0'.                        
018700     03  RUBRAD1-LISTNR PIC 9.                                            
018800     03  FILLER      PIC X(3)    VALUE SPACE.                             
018900     03  FILLER      PIC X(20)   VALUE 'MOTTAGNINGSRAPPORTER'.            
019000     03  RUBRAD1-JUSTERING  PIC X(10).                                    
019100     03  FILLER      PIC X(17)   VALUE SPACE.                             
019110     03  FILLER      PIC X(9)    VALUE 'ANSK-GRP '.                       
019120     03  RUBRAD1-ANSKGRP.                                                 
019130        05 RUBRAD1-ANSK       PIC X(3).                                   
019140        05 FILLER             PIC X(1)  VALUE SPACE.                      
019150        05 RUBRAD1-GRP        PIC X(3).                                   
019160     03  FILLER      PIC X(3)    VALUE  SPACE.                            
019200     03  RUBRAD1-DATUM  PIC 9(6).                                         
019300     03  FILLER      PIC X(2)    VALUE SPACE.                             
019400     03  FILLER      PIC X(4)    VALUE 'SID'.                             
019500     03  RUBRAD1-SIDA PIC ZZ9.                                            
019600     SKIP3                                                                
019700 01  RUBRAD2.                                                             
019800     03  FILLER      PIC X(15)   VALUE  ' ANSK CL  LEVNR'.                
019900     03  FILLER      PIC X(5)    VALUE SPACE.                             
020000     03  FILLER      PIC X(24) VALUE  'ARTNR   KT  RT   ANTAL  '.         
020100     03  FILLER      PIC X(21)   VALUE  'AVSDAT  VÄRDE P INL  '.          
020200     03  FILLER      PIC X(17)   VALUE  'LÖPNUMMER   AVINR'.              
020300     EJECT                                                                
020400 01  DETRAD.                                                              
020500     03  FILLER                   PIC X(2)   VALUE SPACE.                 
020600     03  DETRAD-IDANSKNR          PIC ZZZ.                                
020700     03  FILLER                   PIC X(2)   VALUE SPACE.                 
020800     03  DETRAD-KDCLAGER-S        PIC Z.                                  
020900     03  FILLER                   PIC X(2)   VALUE SPACE.                 
021000     03  DETRAD-IDLEVNR-INL       PIC X(5).                               
021100     03  FILLER                   PIC X(2)   VALUE SPACE.                 
021200     03  DETRAD-IDARTNR           PIC 9(8).                               
021300     03  FILLER                   PIC X(3)   VALUE SPACE.                 
021400     03  DETRAD-W-IDPTYP          PIC XX.                                 
021500     03  FILLER                   PIC X(2)   VALUE SPACE.                 
021600     03  DETRAD-KDRT              PIC Z9.                                 
021700     03  FILLER                   PIC X(1)   VALUE SPACE.                 
021800     03  DETRAD-W-ANTAL           PIC -(6)9.                              
021900     03  FILLER                   PIC X(2)   VALUE SPACE.                 
022000     03  DETRAD-TIAVSDAT          PIC 9(6).                               
022100     03  FILLER                   PIC X(1)   VALUE SPACE.                 
022200     03  DETRAD-W-VARDE           PIC -(8)9.99.                           
022300     03  FILLER                   PIC X(2)   VALUE SPACE.                 
022400     03  DETRAD-IDLOPNR           PIC 9(7)B9.                             
022500     03  FILLER                   PIC X(1)   VALUE SPACE.                 
022600     03  DETRAD-IDAVINR           PIC Z9(6).                              
022700     SKIP2                                                                
022800 01  SUMMA-LEVNR.                                                         
022900     03  FILLER                   PIC X(51)  VALUE SPACE.                 
023000     03  SLEV-VARDEX              PIC +(8)9.99.                           
023100     03  FILLER           PIC X(17) VALUE '  SUMMA PER LEVNR'.            
023200     SKIP3                                                                
023300 01  SUMMA-CLAGER.                                                        
023400     03  FILLER                   PIC X(51)  VALUE SPACE.                 
023500     03  SCLAG-VARDEX             PIC +(8)9.99.                           
023600     03  FILLER          PIC X(19)  VALUE '  SUMMA PER C-LAGER'.          
023700     EJECT                                                                
023800 01  SUMMA-ANSKNR1.                                                       
023900     03  FILLER                   PIC X(51)  VALUE SPACE.                 
024000     03  SANSK-VARDEX             PIC +(8)9.99.                           
024100     03  FILLER          PIC X(16)  VALUE '  SUMMA PER ANSK'.             
024200 01  SUMMA-ANSKNR2.                                                       
024300     03  FILLER                   PIC X(51)  VALUE SPACE.                 
024400     03  SANSK-RT7X               PIC +(8)9.99.                           
024500     03  FILLER      PIC X(18)  VALUE  '    DÄRAV FÖR RT 7'.              
024600 01  SUMMA-ANSKNR3.                                                       
024700     03  FILLER                   PIC X(51)  VALUE SPACE.                 
024800     03  SANSK-RT9X               PIC +(8)9.99.                           
024900     03  FILLER                   PIC X(40)                               
025000         VALUE '    DÄRAV FÖR RT 9, LEVNR 1001,1003,1012'.                
025100 01  SUMMA-ANSKNR4.                                                       
025200     03  FILLER                   PIC X(51)  VALUE SPACE.                 
025300     03  SANSK-RT9-OVWX           PIC +(8)9.99.                           
025400     03  FILLER                   PIC X(14)  VALUE SPACE.                 
025500     03  FILLER      PIC X(18) VALUE 'RT 9, ÖVRIGA LEVNR'.                
025600     SKIP3                                                                
025700 01  SUMMA-GRUPP1.                                                        
025800     03  FILLER                   PIC X(51)  VALUE SPACE.                 
025900     03  SGRP-VARDEX              PIC +(8)9.99.                           
026000     03  FILLER          PIC X(14)  VALUE '  SUMMA GRUPP '.               
026100     03  SUMMA-W-GRP-CTRL         PIC X(7).                               
026200 01  SUMMA-GRUPP2.                                                        
026300     03  FILLER                   PIC X(51)  VALUE SPACE.                 
026400     03  SGRP-RT7X                PIC +(8)9.99.                           
026500     03  FILLER      PIC X(18)  VALUE  '    DÄRAV FÖR RT 7'.              
026600 01  SUMMA-GRUPP3.                                                        
026700     03  FILLER                   PIC X(51)  VALUE SPACE.                 
026800     03  SGRP-RT9X                PIC +(8)9.99.                           
026900     03  FILLER      PIC X(40)                                            
027000         VALUE '    DÄRAV FÖR RT 9, LEVNR 1001,1003,1012'.                
027100 01  SUMMA-GRUPP4.                                                        
027200     03  FILLER                   PIC X(51)  VALUE SPACE.                 
027300     03  SGRP-RT9-OVWX            PIC +(8)9.99.                           
027400     03  FILLER                   PIC X(14)  VALUE SPACE.                 
027500     03  FILLER      PIC X(18) VALUE 'RT 9, ÖVRIGA LEVNR'.                
027600     EJECT                                                                
027700 01  SUMMA-SEKTION1.                                                      
027800     03  FILLER                    PIC X(51)  VALUE SPACE.                
027900     03  SSEKT-VARDEX              PIC +(8)9.99.                          
028000     03  FILLER          PIC X(16)  VALUE '  SUMMA SEKTION '.             
028100     03  SUMMA-W-SEKT-CTRL         PIC X(8).                              
028200 01  SUMMA-SEKTION2.                                                      
028300     03  FILLER                    PIC X(51)  VALUE SPACE.                
028400     03  SSEKT-RT7X                PIC +(8)9.99.                          
028500     03  FILLER      PIC X(18)  VALUE  '    DÄRAV FÖR RT 7'.              
028600 01  SUMMA-SEKTION3.                                                      
028700     03  FILLER                    PIC X(51)  VALUE SPACE.                
028800     03  SSEKT-RT9X                PIC +(8)9.99.                          
028900     03  FILLER                    PIC X(40)                              
029000         VALUE '    DÄRAV FÖR RT 9, LEVNR 1001,1003,1012'.                
029100 01  SUMMA-SEKTION3A.                                                     
029200     03  FILLER                    PIC X(51)  VALUE SPACE.                
029300     03  SSEKT-RT9-OVWX            PIC +(8)9.99.                          
029400     03  FILLER                    PIC X(14)  VALUE SPACE.                
029500     03  FILLER      PIC X(18) VALUE 'RT 9, ÖVRIGA LEVNR'.                
029600     SKIP3                                                                
029700 01  SUMMA-TOTALT1.                                                       
029800     03  FILLER                    PIC X(51)  VALUE SPACE.                
029900     03  SUMMA-TOTALT-W-TOT-VARDE  PIC +(8)9.99.                          
030000     03  FILLER          PIC X(9)    VALUE  '  TOTALT '.                  
030100     03  SUMMA-W-TOT-CTRL          PIC X(8).                              
030200 01  SUMMA-TOTALT2.                                                       
030300     03  FILLER                    PIC X(51)  VALUE SPACE.                
030400     03  SUMMA-TOTALT-W-TOT-RT7    PIC +(8)9.99.                          
030500     03  FILLER      PIC X(18)  VALUE  '    DÄRAV FÖR RT 7'.              
030600 01  SUMMA-TOTALT3.                                                       
030700     03  FILLER                    PIC X(51)  VALUE SPACE.                
030800     03  SUMMA-TOTALT-W-TOT-RT9    PIC +(8)9.99.                          
030900     03  FILLER                    PIC X(40)                              
031000         VALUE '    DÄRAV FÖR RT 9, LEVNR 1001,1003,1012'.                
031100 01  SUMMA-TOTALT4.                                                       
031200     03  FILLER                    PIC X(51)  VALUE SPACE.                
031300     03  SUMMA-TOTALT-W-TOT-RT9-OVR  PIC +(8)9.99.                        
031400     03  FILLER                    PIC X(14)  VALUE SPACE.                
031500     03  FILLER      PIC X(18) VALUE 'RT 9, ÖVRIGA LEVNR'.                
031600     EJECT                                                                
031700 01  SUMMA-IDAFFOMR1.                                                     
031800     03  FILLER                   PIC X(51)  VALUE SPACE.                 
031900     03  SIDAFFOMR-VARDEX         PIC +(8)9.99.                           
032000     03  FILLER   PIC X(25) VALUE '  SUMMA PER AFFÄRSOMRÅDE '.            
032100     03  SIDAFFOMR-ID             PIC 9(3)   VALUE ZERO.                  
032200     03  FILLER                   PIC X(1)   VALUE SPACE.                 
032300     03  SIDAFFOMR-TEXT           PIC X(8).                               
032400     SKIP3                                                                
032500 01  SUMMA-IDAFFOMR2.                                                     
032600     03  FILLER                   PIC X(51)  VALUE SPACE.                 
032700     03  SOMR-RT7X                PIC +(8)9.99.                           
032800     03  FILLER      PIC X(18)  VALUE  '    DÄRAV FÖR RT 7'.              
032900 01  SUMMA-IDAFFOMR3.                                                     
033000     03  FILLER                   PIC X(51)  VALUE SPACE.                 
033100     03  SOMR-RT9X                PIC +(8)9.99.                           
033200     03  FILLER                   PIC X(40)                               
033300         VALUE '    DÄRAV FÖR RT 9, LEVNR 1001,1003,1012'.                
033400 01  SUMMA-IDAFFOMR4.                                                     
033500     03  FILLER                   PIC X(51)  VALUE SPACE.                 
033600     03  SOMR-RT9-OVWX            PIC +(8)9.99.                           
033700     03  FILLER                   PIC X(14)  VALUE SPACE.                 
033800     03  FILLER      PIC X(18) VALUE 'RT 9, ÖVRIGA LEVNR'.                
033900     SKIP3                                                                
034000 01  SUM-VARIABLER.                                                       
034100     03  SLEV-VARDE               PIC S9(9)V99  COMP-3 VALUE +0.          
034200     03  SCLAG-VARDE              PIC S9(9)V99  COMP-3 VALUE +0.          
034300     03  SANSK-VARDE              PIC S9(9)V99  COMP-3 VALUE +0.          
034400     03  SANSK-RT7                PIC S9(9)V99  COMP-3 VALUE +0.          
034500     03  SANSK-RT9                PIC S9(9)V99  COMP-3 VALUE +0.          
034600     03  SANSK-RT9-OVR            PIC S9(9)V99  COMP-3 VALUE +0.          
034700     03  SGRP-VARDE               PIC S9(9)V99  COMP-3 VALUE +0.          
034800     03  SGRP-RT7                 PIC S9(9)V99  COMP-3 VALUE +0.          
034900     03  SGRP-RT9                 PIC S9(9)V99  COMP-3 VALUE +0.          
035000     03  SGRP-RT9-OVR             PIC S9(9)V99  COMP-3 VALUE +0.          
035100     03  SSEKT-VARDE              PIC S9(9)V99  COMP-3 VALUE +0.          
035200     03  SSEKT-RT7                PIC S9(9)V99  COMP-3 VALUE +0.          
035300     03  SSEKT-RT9                PIC S9(9)V99  COMP-3 VALUE +0.          
035400     03  SSEKT-RT9-OVR            PIC S9(9)V99  COMP-3 VALUE +0.          
035500     03  SIDAFFOMR                PIC S9(9)V99  COMP-3 VALUE +0.          
035600     03  SOMR-RT7                 PIC S9(9)V99  COMP-3 VALUE +0.          
035700     03  SOMR-RT9                 PIC S9(9)V99  COMP-3 VALUE +0.          
035800     03  SOMR-RT9-OVR             PIC S9(9)V99  COMP-3 VALUE +0.          
035900     EJECT                                                                
036000 PROCEDURE DIVISION.                                                      
036200 STYR SECTION.                                                            
036300                                                                          
036400     ACCEPT W-DATUM FROM DATE                                             
036600                                                                          
036700     SORT SRTFIL                                                          
036800     ASCENDING KEY SRT-SORTFLT1 SRT-IDANSKNR  SRT-KDCLAGER-S              
036900     SRT-IDLEVNR-INL SRT-IDARTNR-S SRT-IDLOPNR SRT-IDPTYP                 
037000     USING  INFILER                                                       
037100     OUTPUT PROCEDURE A-OUTPUT-FAS3                                       
037200                                                                          
037300     IF SORT-RETURN = 16                                                  
037400         DISPLAY SORTFEL                                                  
037900         CALL ABEND USING RKOD                                            
038000     ELSE                                                                 
038100         MOVE ZERO TO RETURN-CODE                                         
038200         GOBACK                                                           
038300     END-IF                                                               
038310     .                                                                    
038400     EJECT                                                                
038500 A-OUTPUT-FAS3 SECTION.                                                   
038600*                                                                         
038700******* B E A R B E T N I N G ************************************        
038800*                                                                         
038900*    OM EN PTYP 310 FÖLJS AV EN PTYP 320 MED SAMMA LÖPNR OCH              
039000*        PTYP 320 ÄR MÄRKT FÖR MARKULERING(KVANTAV=2),DELETAS             
039100*        BÅDA POSTERNA.                                                   
039200*    SAMTLIGA INTRANS, FÖRUTOM PT 310,320,340 MED REDOVISN.TYP > 5        
039300*    SKRIVS PÅ UTFIL I 7074-FORMAT (INLEDANDE ^  SAMT ÖVERSTANS           
039400*    I VAR TIONDE POSITION).                                              
039500*                                                                         
039600*    EJ DELETADE PT310,320,340 SKRIVS UT PÅ LISTA W61195-01               
039700*        ELLER LISTA W61195-02  BEROENDE PÅ SORTFLT1 (=00 RESP 01)        
039800*        FÖR PT320 SKRIVS POSTEN UT ENDAST OM MOTTAGET ANTAL              
039900*        AVVIKER FRÅN AVISERAT ANTAL.                                     
040000*    SUMMERING AV VÄRDE-P-INL SKER PER...LEVERANTÖR                       
040100*                                        CLAGER                           
040200*                                        ANSKAFFARE  (SIDBYTE)            
040300*                                        GRUPP                            
040400*                                        SEKTION                          
040500*                                        TOTALT                           
040600*                                                                         
040700*    SAMMA DDNAMN ANVÄNDES FÖR LISTA W61195-01 OCH W61195-02              
040800****************************************************************          
040900     SKIP3                                                                
041000     OPEN OUTPUT LISTORNA W61195                                          
041100     PERFORM S03-NOLLSTALL                                                
041200*                                                                         
041300     MOVE 'W61195' TO PS-PROGNAMN                                         
041400     MOVE 'W61195' TO PS-FDNAMN                                           
041500     MOVE 'W61195D2' TO PS-DDNAMN2                                        
041600*                                                                         
041700     PERFORM UNTIL EOF-SRTFIL = JA                                        
041800*                                                                         
041900     IF FLAG-DELETE-310-320  = JA                                         
042000     RETURN   SRTFIL   AT END                                             
042100         MOVE JA TO  EOF-SRTFIL                                           
042200     END-IF                                                               
042300*                                                                         
042400     IF EOF-SRTFIL = NEJ                                                  
042500        MOVE SRT-POST TO W-AREA                                           
042600        RETURN SRTFIL   AT END                                            
042700            MOVE JA TO EOF-SRTFIL                                         
042800            MOVE 999 TO SRT-IDPTYP                                        
042900        END-RETURN                                                        
043000*                                                                         
043100        IF IDPTYP = 310  AND SRT-IDPTYP = 320                             
043200              AND IDLOPNR = SRT-IDLOPNR  AND  SRT-KDAVVANT = 2            
043300              MOVE JA TO FLAG-DELETE-310-320                              
043400        ELSE                                                              
043500           MOVE NEJ  TO FLAG-DELETE-310-320                               
043600           IF IDPTYP = 310 OR 340 OR                                      
043700              IDPTYP = 320 AND  KVAVIS NOT = KVMOTANT                     
043800              PERFORM       AB-SKRIV-LISTA-01-02                          
043900           END-IF                                                         
044000           IF (IDPTYP = 310 OR 320 OR 340) AND KDRT > 5                   
044100              CONTINUE                                                    
044200           ELSE                                                           
044300             PERFORM AC-SKRIV-W61195                                      
044400             MOVE IDPTYP TO  PS-TRANSTYP                                  
044500             CALL POSTSUM USING PS-PARM                                   
044600           END-IF                                                         
044700        END-IF                                                            
044800     END-IF                                                               
044900     END-PERFORM                                                          
045000*                                                                         
045100     PERFORM S04-SUMMA-LEVNR                                              
045200     PERFORM S05-SUMMA-CLAGER                                             
045300     PERFORM S06-SUMMA-ANSKNR                                             
045400     PERFORM S07-SUMMA-GRUPP                                              
045500     PERFORM S08-SUMMA-SEKTION                                            
045600     PERFORM S09-SUMMA-TOTALT                                             
045700     PERFORM S11-SUMMA-IDAFFOMR                                           
045800*                                                                         
045900     MOVE 'S' TO PS-OPKOD                                                 
046000     CALL POSTSUM  USING PS-PARM                                          
046100*                                                                         
046200     PERFORM AC-SKRIV-W61195                                              
046300*                                                                         
046400     CLOSE LISTORNA W61195.                                               
046500     EJECT                                                                
046600 AB-SKRIV-LISTA-01-02    SECTION.                                         
046700*                                                                         
046800* BRYTPUNKTER OCH SUMMERINGAR *                                           
046900*                                                                         
047000     MOVE IDANSKNR TO ANSKGRP-IDANSK                                      
047100     CALL W200ANSK USING ANSKGRP-W009W42                                  
047200*                                                                         
047300     MOVE ANSKGRP-GRUPP-INTERVALL TO W-GRP-CTRL                           
047301***************************************************************           
047310                                     RUBRAD1-ANSKGRP                      
047320***************************************************************           
047400     MOVE ANSKGRP-TESEKT TO W-SEKT-CTRL                                   
047500     MOVE ANSKGRP-TEFUNK TO W-TOT-CTRL                                    
047600     MOVE ANSKGRP-IDAFFOMR TO W-IDAFFOMR                                  
047700     MOVE ANSKGRP-TEAFFOMR TO W-TEAFFOMR                                  
047800*                                                                         
047900     IF BRYT-START = JA                                                   
048000        PERFORM S10-BRYTPUNKTER                                           
048100        MOVE NEJ TO BRYT-START                                            
048200     END-IF                                                               
048300     IF BRYT-ANSK NOT = IDANSKNR                                          
048310        MOVE BRYT-GRP TO RUBRAD1-ANSKGRP                                  
048400        PERFORM S04-SUMMA-LEVNR                                           
048500        PERFORM S05-SUMMA-CLAGER                                          
048600        PERFORM S06-SUMMA-ANSKNR                                          
048610        MOVE W-GRP-CTRL TO RUBRAD1-ANSKGRP                                
048700     ELSE                                                                 
048800        IF BRYT-CL NOT = KDCLAGER-S                                       
048810           MOVE BRYT-GRP TO RUBRAD1-ANSKGRP                               
048900           PERFORM S04-SUMMA-LEVNR                                        
049000           PERFORM S05-SUMMA-CLAGER                                       
049010           MOVE W-GRP-CTRL TO RUBRAD1-ANSKGRP                             
049100        ELSE                                                              
049200           IF BRYT-LEVNR NOT = IDLEVNR-INL                                
049210              MOVE BRYT-GRP TO RUBRAD1-ANSKGRP                            
049300              PERFORM S04-SUMMA-LEVNR                                     
049310              MOVE W-GRP-CTRL TO RUBRAD1-ANSKGRP                          
049400           END-IF                                                         
049500        END-IF                                                            
049600     END-IF                                                               
049700     IF BRYT-GRP NOT = W-GRP-CTRL                                         
049800        PERFORM S07-SUMMA-GRUPP                                           
049900     END-IF                                                               
050000     IF BRYT-SEKT NOT = W-SEKT-CTRL                                       
050100        PERFORM S08-SUMMA-SEKTION                                         
050200     END-IF                                                               
050300     IF BRYT-TOT NOT = W-TOT-CTRL                                         
050400        PERFORM S09-SUMMA-TOTALT                                          
050500     END-IF                                                               
050600     IF BRYT-IDAFFOMR NOT = W-IDAFFOMR                                    
050700        PERFORM S11-SUMMA-IDAFFOMR                                        
050800     END-IF                                                               
050900*                                                                         
051000     IF NOT (BRYT-GRP = W-GRP-CTRL) OR NOT                                
051100        (BRYT-SEKT = W-SEKT-CTRL) OR NOT (BRYT-TOT = W-TOT-CTRL)          
051200           MOVE +99 TO RAD                                                
051300     END-IF                                                               
051400*                                                                         
051500     MOVE IDPTYP TO W-IDPTYP                                              
051600     IF IDPTYP = 320                                                      
051700         COMPUTE W-ANTAL = KVMOTANT - KVAVIS                              
051800     ELSE                                                                 
051900         MOVE KVAVIS TO W-ANTAL                                           
052000     END-IF                                                               
052100     COMPUTE W-VARDE = W-ANTAL * PRARTSTD                                 
052200     ADD W-VARDE TO W-TOT-VARDE                                           
052300     MOVE ZERO TO  W-VARDE-RT7  W-VARDE-RT9                               
052400     MOVE ZERO TO W-VARDE-RT9-OVR                                         
052500     IF KDRT = 7                                                          
052600     MOVE  W-VARDE TO W-VARDE-RT7                                         
052700     ADD   W-VARDE TO W-TOT-RT7                                           
052800     ELSE                                                                 
052900     IF KDRT = 9                                                          
053000       IF IDLEVNR-INL =  '1001 ' OR '1003 ' OR '1012 ' OR                 
053010                         'BL3YA' OR 'BP2TH'                               
053100     MOVE W-VARDE TO W-VARDE-RT9                                          
053200     ADD  W-VARDE TO W-TOT-RT9                                            
053300       ELSE                                                               
053400       MOVE W-VARDE TO W-VARDE-RT9-OVR                                    
053500       ADD  W-VARDE TO W-TOT-RT9-OVR                                      
053600     END-IF                                                               
053700     END-IF                                                               
053800     END-IF                                                               
053900*                                                                         
054000     ADD W-VARDE TO SIDAFFOMR                                             
054100     ADD W-VARDE TO SLEV-VARDE                                            
054200     ADD W-VARDE TO SCLAG-VARDE                                           
054300     ADD W-VARDE TO SANSK-VARDE                                           
054400     ADD W-VARDE-RT7 TO SANSK-RT7                                         
054500     ADD W-VARDE-RT9 TO SANSK-RT9                                         
054600     ADD W-VARDE-RT9-OVR TO SANSK-RT9-OVR                                 
054700     ADD W-VARDE TO SGRP-VARDE                                            
054800     ADD W-VARDE-RT7 TO SGRP-RT7                                          
054900     ADD W-VARDE-RT9 TO SGRP-RT9                                          
055000     ADD W-VARDE-RT9-OVR TO SGRP-RT9-OVR                                  
055100     ADD W-VARDE TO SSEKT-VARDE                                           
055200     ADD W-VARDE-RT7 TO SSEKT-RT7                                         
055300                        SOMR-RT7                                          
055400     ADD W-VARDE-RT9 TO SSEKT-RT9                                         
055500                        SOMR-RT9                                          
055600     ADD W-VARDE-RT9-OVR TO SSEKT-RT9-OVR                                 
055700                        SOMR-RT9-OVR                                      
055800     EJECT                                                                
055900*** TEST OM LISTA 02 SKA BÖRJA                                            
056000*                                                                         
056100     IF SORTFLT1 = 01 AND SW-W61195-02 = NEJ                              
056200         MOVE +0 TO SIDA                                                  
056300         MOVE +99 TO RAD                                                  
056400         MOVE JA  TO SW-W61195-02                                         
056500         MOVE '-JUSTERING' TO W-JUSTERING-RUB                             
056600         MOVE 2  TO W-LISTNR-RUB                                          
056700        MOVE W-VARDE          TO W-TOT-VARDE                              
056800        MOVE W-VARDE-RT7      TO W-TOT-RT7                                
056900        MOVE W-VARDE-RT9      TO W-TOT-RT9                                
057000        MOVE W-VARDE-RT9-OVR  TO W-TOT-RT9-OVR                            
057100        PERFORM S03-NOLLSTALL                                             
057200     END-IF                                                               
057300*                                                                         
057400*                                                                         
057500     PERFORM S02-SKRIV-DETRAD                                             
057600*                                                                         
057700     PERFORM S10-BRYTPUNKTER.                                             
057800     EJECT                                                                
057900 AC-SKRIV-W61195 SECTION.                                                 
058000*                                                                         
058100     WRITE UT-POST FROM W-AREA.                                           
058200     EJECT                                                                
058300 S01-SKRIV-RUBRIK SECTION.                                                
058400     SKIP2                                                                
058500     MOVE +0 TO RAD                                                       
058600     ADD +1 TO SIDA                                                       
058700     MOVE W-LISTNR-RUB TO RUBRAD1-LISTNR                                  
058800     MOVE W-JUSTERING-RUB TO RUBRAD1-JUSTERING                            
058900     MOVE W-DATUM TO RUBRAD1-DATUM                                        
059000     MOVE SIDA TO RUBRAD1-SIDA                                            
059100     MOVE RUBRAD1 TO LISTRAD                                              
059200     WRITE W61195-00X-POST FROM LISTAREA AFTER NYSIDA                     
059300     ADD +1 TO RAD                                                        
059400     SKIP2                                                                
059500     MOVE RUBRAD2 TO LISTRAD                                              
059600     WRITE W61195-00X-POST FROM LISTAREA AFTER 3                          
059700     ADD +3 TO RAD                                                        
059800     SKIP1                                                                
059900     MOVE JA TO BRYT.                                                     
060000     EJECT                                                                
060100 S02-SKRIV-DETRAD SECTION.                                                
060200     SKIP2                                                                
060300     IF RAD > +36                                                         
060400        PERFORM S01-SKRIV-RUBRIK                                          
060500     END-IF                                                               
060600     MOVE IDARTNR TO DETRAD-IDARTNR                                       
060700     MOVE W-IDPTYP TO DETRAD-W-IDPTYP                                     
060800     MOVE KDRT TO DETRAD-KDRT                                             
060900     MOVE W-ANTAL TO DETRAD-W-ANTAL                                       
061000     MOVE TIAVSDAT TO DETRAD-TIAVSDAT                                     
061100     MOVE W-VARDE TO DETRAD-W-VARDE                                       
061200     MOVE IDLOPNR TO DETRAD-IDLOPNR                                       
061300     MOVE IDAVINR TO DETRAD-IDAVINR                                       
061400     IF BRYT = JA                                                         
061500        MOVE IDANSKNR TO DETRAD-IDANSKNR                                  
061600        MOVE KDCLAGER-S TO DETRAD-KDCLAGER-S                              
061700        MOVE IDLEVNR-INL TO DETRAD-IDLEVNR-INL                            
061800        MOVE DETRAD TO LISTRAD                                            
061900        WRITE W61195-00X-POST FROM LISTAREA AFTER 2                       
062000        ADD +2 TO RAD                                                     
062100        MOVE NEJ TO BRYT                                                  
062200     ELSE                                                                 
062300        IF (BRYT-ANSK = IDANSKNR) OR (BRYT-CL = KDCLAGER-S)               
062400           OR (BRYT-LEVNR = IDLEVNR-INL)                                  
062500              MOVE ZERO  TO DETRAD-IDANSKNR                               
062600              MOVE ZERO  TO DETRAD-KDCLAGER-S                             
062700              MOVE SPACE TO DETRAD-IDLEVNR-INL                            
062800              MOVE DETRAD TO LISTRAD                                      
062900              WRITE W61195-00X-POST FROM LISTAREA AFTER 1                 
063000              ADD +1 TO RAD                                               
063100        END-IF                                                            
063200     END-IF.                                                              
063300     EJECT                                                                
063400 S03-NOLLSTALL SECTION.                                                   
063500     SKIP2                                                                
063600     MOVE ZERO TO SIDAFFOMR                                               
063700     MOVE ZERO TO SLEV-VARDE                                              
063800     MOVE ZERO TO SCLAG-VARDE                                             
063900     MOVE ZERO TO SANSK-VARDE                                             
064000     MOVE ZERO TO SANSK-RT7                                               
064100     MOVE ZERO TO SANSK-RT9                                               
064200     MOVE ZERO TO SANSK-RT9-OVR                                           
064300     MOVE ZERO TO SGRP-VARDE                                              
064400     MOVE ZERO TO SGRP-RT7                                                
064500     MOVE ZERO TO SGRP-RT9                                                
064600     MOVE ZERO TO SGRP-RT9-OVR                                            
064700     MOVE ZERO TO SSEKT-VARDE                                             
064800     MOVE ZERO TO SSEKT-RT7                                               
064900     MOVE ZERO TO SSEKT-RT9                                               
065000     MOVE ZERO TO SSEKT-RT9-OVR.                                          
065100     EJECT                                                                
065200 S04-SUMMA-LEVNR SECTION.                                                 
065300     SKIP2                                                                
065400     IF RAD > +36                                                         
065500        PERFORM S01-SKRIV-RUBRIK                                          
065600     END-IF                                                               
065700     MOVE SLEV-VARDE TO SLEV-VARDEX                                       
065800     MOVE SUMMA-LEVNR TO LISTRAD                                          
065900     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
066000     ADD +1 TO RAD                                                        
066100     MOVE ZERO TO SLEV-VARDE                                              
066200     MOVE JA TO BRYT.                                                     
066300     EJECT                                                                
066400 S05-SUMMA-CLAGER SECTION.                                                
066500     SKIP2                                                                
066600     IF RAD > +36                                                         
066700        PERFORM S01-SKRIV-RUBRIK                                          
066800     END-IF                                                               
066900     MOVE SCLAG-VARDE TO SCLAG-VARDEX                                     
067000     MOVE SUMMA-CLAGER TO LISTRAD                                         
067100     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
067200     ADD +1 TO RAD                                                        
067300     MOVE ZERO TO SCLAG-VARDE                                             
067400     MOVE JA TO BRYT.                                                     
067500     EJECT                                                                
067600 S06-SUMMA-ANSKNR SECTION.                                                
067700     SKIP2                                                                
067800     IF RAD > +36                                                         
067900        PERFORM S01-SKRIV-RUBRIK                                          
068000     END-IF                                                               
068100     MOVE SANSK-VARDE TO SANSK-VARDEX                                     
068200     MOVE SANSK-RT7 TO SANSK-RT7X                                         
068300     MOVE SANSK-RT9 TO SANSK-RT9X                                         
068400     MOVE SANSK-RT9-OVR TO SANSK-RT9-OVWX                                 
068500     MOVE SUMMA-ANSKNR1 TO LISTRAD                                        
068600     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
068700     ADD +1 TO RAD                                                        
068800     SKIP1                                                                
068900     MOVE SUMMA-ANSKNR2 TO LISTRAD                                        
069000     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
069100     ADD +1 TO RAD                                                        
069200     SKIP1                                                                
069300     MOVE SUMMA-ANSKNR3 TO LISTRAD                                        
069400     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
069500     ADD +1 TO RAD                                                        
069600     SKIP1                                                                
069700     MOVE SUMMA-ANSKNR4 TO LISTRAD                                        
069800     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
069900     MOVE +99 TO RAD                                                      
070000     MOVE ZERO TO SANSK-VARDE                                             
070100     MOVE ZERO TO SANSK-RT7                                               
070200     MOVE ZERO TO SANSK-RT9                                               
070300     MOVE ZERO TO SANSK-RT9-OVR                                           
070400     MOVE JA TO BRYT.                                                     
070500     EJECT                                                                
070600 S07-SUMMA-GRUPP SECTION.                                                 
070700     SKIP2                                                                
070701****************************************************************          
070710     MOVE BRYT-GRP TO RUBRAD1-ANSKGRP                                     
070720****************************************************************          
070800     PERFORM S01-SKRIV-RUBRIK                                             
070810     MOVE ANSKGRP-GRUPP-INTERVALL TO W-GRP-CTRL                           
070820***************************************************************           
070830                                     RUBRAD1-ANSKGRP                      
070840***************************************************************           
070900     SKIP1                                                                
071000     MOVE SGRP-VARDE TO SGRP-VARDEX                                       
071100     MOVE SGRP-RT7 TO SGRP-RT7X                                           
071200     MOVE SGRP-RT9 TO SGRP-RT9X                                           
071300     MOVE SGRP-RT9-OVR TO SGRP-RT9-OVWX                                   
071301     MOVE BRYT-GRP TO SUMMA-W-GRP-CTRL                                    
071500     MOVE SUMMA-GRUPP1 TO LISTRAD                                         
071600     WRITE W61195-00X-POST FROM LISTAREA AFTER 2                          
071700     ADD +2 TO RAD                                                        
071800     SKIP1                                                                
071900     MOVE SUMMA-GRUPP2 TO LISTRAD                                         
072000     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
072100     ADD +1 TO RAD                                                        
072200     SKIP1                                                                
072300     MOVE SUMMA-GRUPP3 TO LISTRAD                                         
072400     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
072500     ADD +1 TO RAD                                                        
072600     SKIP1                                                                
072700     MOVE SUMMA-GRUPP4 TO LISTRAD                                         
072800     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
072900     ADD +1 TO RAD                                                        
073000     MOVE ZERO TO SGRP-VARDE                                              
073100     MOVE ZERO TO SGRP-RT7                                                
073200     MOVE ZERO TO SGRP-RT9                                                
073300     MOVE ZERO TO SGRP-RT9-OVR                                            
073400     MOVE JA TO BRYT.                                                     
073500     EJECT                                                                
073600 S08-SUMMA-SEKTION SECTION.                                               
073700     SKIP2                                                                
073800     MOVE SSEKT-VARDE TO SSEKT-VARDEX                                     
073900     MOVE SSEKT-RT7 TO SSEKT-RT7X                                         
074000     MOVE SSEKT-RT9 TO SSEKT-RT9X                                         
074100     MOVE SSEKT-RT9-OVR TO SSEKT-RT9-OVWX                                 
074200     MOVE BRYT-SEKT TO SUMMA-W-SEKT-CTRL                                  
074300     MOVE SUMMA-SEKTION1 TO LISTRAD                                       
074400     WRITE W61195-00X-POST FROM LISTAREA AFTER 3                          
074500     ADD +3 TO RAD                                                        
074600     SKIP1                                                                
074700     MOVE SUMMA-SEKTION2 TO LISTRAD                                       
074800     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
074900     ADD +1 TO RAD                                                        
075000     SKIP1                                                                
075100     MOVE SUMMA-SEKTION3 TO LISTRAD                                       
075200     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
075300     ADD +1 TO RAD                                                        
075400     SKIP1                                                                
075500     MOVE SUMMA-SEKTION3A TO LISTRAD                                      
075600     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
075700     ADD +1 TO RAD                                                        
075800     MOVE ZERO TO SSEKT-VARDE                                             
075900     MOVE ZERO TO SSEKT-RT7                                               
076000     MOVE ZERO TO SSEKT-RT9                                               
076100     MOVE ZERO TO SSEKT-RT9-OVR                                           
076200     MOVE JA TO BRYT.                                                     
076300     EJECT                                                                
076400 S09-SUMMA-TOTALT SECTION.                                                
076500     SKIP2                                                                
076600******     F U N K T I O N    *******                                     
076700                                                                          
076800     MOVE W-TOT-VARDE TO SUMMA-TOTALT-W-TOT-VARDE                         
076900     MOVE W-TOT-RT7 TO SUMMA-TOTALT-W-TOT-RT7                             
077000     MOVE W-TOT-RT9 TO SUMMA-TOTALT-W-TOT-RT9                             
077100     MOVE W-TOT-RT9-OVR TO SUMMA-TOTALT-W-TOT-RT9-OVR                     
077200     MOVE BRYT-TOT TO SUMMA-W-TOT-CTRL                                    
077300     MOVE SUMMA-TOTALT1 TO LISTRAD                                        
077400     WRITE W61195-00X-POST FROM LISTAREA AFTER 3                          
077500     ADD +3 TO RAD                                                        
077600     SKIP1                                                                
077700     MOVE SUMMA-TOTALT2 TO LISTRAD                                        
077800     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
077900     ADD +1 TO RAD                                                        
078000     SKIP1                                                                
078100     MOVE SUMMA-TOTALT3 TO LISTRAD                                        
078200     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
078300     ADD +1 TO RAD                                                        
078400     SKIP1                                                                
078500     MOVE SUMMA-TOTALT4 TO LISTRAD                                        
078600     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
078700     ADD +1 TO RAD                                                        
078800                                                                          
078900     MOVE ZERO           TO W-TOT-VARDE                                   
079000                            W-TOT-RT7                                     
079100                            W-TOT-RT9                                     
079200                            W-TOT-RT9-OVR.                                
079300     EJECT                                                                
079400 S10-BRYTPUNKTER SECTION.                                                 
079500     SKIP2                                                                
079600     MOVE W-GRP-CTRL TO BRYT-GRP                                          
079700     MOVE W-SEKT-CTRL TO BRYT-SEKT                                        
079800     MOVE W-IDAFFOMR  TO BRYT-IDAFFOMR                                    
079900     MOVE W-TEAFFOMR  TO BRYT-TEAFFOMR                                    
080000     MOVE W-TOT-CTRL TO BRYT-TOT                                          
080100     MOVE IDANSKNR TO BRYT-ANSK                                           
080200     MOVE KDCLAGER-S TO BRYT-CL                                           
080300     MOVE IDLEVNR-INL TO BRYT-LEVNR.                                      
080400     EJECT                                                                
080500 S11-SUMMA-IDAFFOMR SECTION.                                              
080600     SKIP2                                                                
080700     MOVE SIDAFFOMR       TO SIDAFFOMR-VARDEX                             
080800     MOVE BRYT-TEAFFOMR   TO SIDAFFOMR-TEXT                               
080900     MOVE BRYT-IDAFFOMR   TO SIDAFFOMR-ID                                 
081000     MOVE SUMMA-IDAFFOMR1 TO LISTRAD                                      
081100     WRITE W61195-00X-POST FROM LISTAREA AFTER 2                          
081200     ADD +2 TO RAD                                                        
081300     MOVE ZERO TO SIDAFFOMR                                               
081400                                                                          
081500     MOVE SOMR-RT7        TO SOMR-RT7X                                    
081600     MOVE SUMMA-IDAFFOMR2 TO LISTRAD                                      
081700     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
081800     ADD 1                TO RAD                                          
081900     MOVE ZERO            TO SOMR-RT7                                     
082000                                                                          
082100     MOVE SOMR-RT9        TO SOMR-RT9X                                    
082200     MOVE SUMMA-IDAFFOMR3 TO LISTRAD                                      
082300     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
082400     ADD 1                TO RAD                                          
082500     MOVE ZERO            TO SOMR-RT9                                     
082600                                                                          
082700     MOVE SOMR-RT9-OVR    TO SOMR-RT9-OVWX                                
082800     MOVE SUMMA-IDAFFOMR4 TO LISTRAD                                      
082900     WRITE W61195-00X-POST FROM LISTAREA AFTER 1                          
083000     ADD 1                TO RAD                                          
083100     MOVE ZERO            TO SOMR-RT9-OVR                                 
083200                                                                          
083300     MOVE JA TO BRYT                                                      
083400     .                                                                    
