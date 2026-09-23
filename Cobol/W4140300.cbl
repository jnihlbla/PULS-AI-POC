000103 ID DIVISION.                                                             
000203     SKIP2                                                                
000303 PROGRAM-ID.     W4140300.                                                
000403*AUTHOR.         GUNNAR LARSSON IDK.                                      
000503*DATE-WRITTEN.   92/03/24.                                                
000603                                                                          
000703*    REMARKS.                                                             
000803*                                                                         
000903*    FUNKTION:                                                            
001003*        SKAPAR FILER TILL ANDRA SYSTEM VID ORDERAVSLUT                   
001103*                                                                         
001203*                                                                         
001303*    ABENDKODER:                                                          
001403*        U0016 -  . . . .                                                 
001503*        U1000 -  . . . .                                                 
001603*                                                                         
001703*  2012-07-10  E-TRACKER 8200058 MANAGEMENT SCRAPPING FOLLOW UP           
001800*                                                                         
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ORDERHUVUDEN VID ORDERAVSLUT                               
002700     SELECT W41401                     ASSIGN TO W41403D1.                
002800     SKIP2                                                                
002900*          --- ORDERRADER & ORDERBEKRÄFTELSER VID ORDERAVSLUT             
003000     SELECT W41403                     ASSIGN TO W41403D2.                
003100     SKIP2                                                                
003200*          --- VR                                                         
003300     SELECT W41406                     ASSIGN TO W41403D3.                
003400     SKIP2                                                                
003500*          --- NOAC 001                                                   
003600     SELECT W41407                     ASSIGN TO W41403D4.                
003700     SKIP2                                                                
003800*          --- NOAC 015                                                   
003900     SELECT W41408                     ASSIGN TO W41403D5.                
004000     SKIP2                                                                
004100*          --- NOAC 020                                                   
004200     SELECT W41409                     ASSIGN TO W41403D6.                
004300     SKIP2                                                                
004400*          --- NOAC 002,003,004,005,006,007,008                           
004500     SELECT W4140A                     ASSIGN TO W41403D7.                
004600     SKIP2                                                                
004700*          --- PRISLARM                                                   
004800     SELECT W4140J                     ASSIGN TO W41403D9.                
004900     SKIP2                                                                
005000*          --- SRS-ANNULLATION VORKÖ                                      
005100     SELECT W4140D                     ASSIGN TO W41403DA.                
005200     SKIP2                                                                
005300*          --- BASLAGER                                                   
005400     SELECT W4140E                     ASSIGN TO W41403DB.                
005500     SKIP2                                                                
005600*          --- NOAC 009                                                   
005700     SELECT W4140G                     ASSIGN TO W41403DD.                
005800     SKIP2                                                                
005900*          --- TPO ORDERENTRY O TILLÄGG                                   
006000     SELECT W41405                     ASSIGN TO W41403DE.                
006100     SKIP2                                                                
006200*          --- VOR-KÖ ORDERENTRY                                          
006300     SELECT W4140I                     ASSIGN TO W41403DG.                
006400     SKIP2                                                                
006500*          --- SPX ORDERRADER                                             
006600     SELECT W414X2                     ASSIGN TO W41403DI.                
006700     SKIP2                                                                
006800*          --- SPX ORDERBEKRÄFTELSER                                      
006900     SELECT W414X3                     ASSIGN TO W41403DJ.                
007005     SKIP2                                                                
007105*          --- SKROT ORDERRADER                                           
007210     SELECT W4140S                     ASSIGN TO W41403DK.                
007305     EJECT                                                                
007405 DATA DIVISION.                                                           
007505     SKIP3                                                                
007605 FILE SECTION.                                                            
007705     SKIP3                                                                
007805 FD  W41401                                                               
007905     RECORDING       F                                                    
008005     BLOCK CONTAINS  0.                                                   
008105     SKIP2                                                                
008205*01  -COPY W414001      -L.                                               
008305     SKIP3                                                                
008405 FD  W41403                                                               
008505     RECORDING       V                                                    
008605     BLOCK CONTAINS  0.                                                   
008705     SKIP2                                                                
008805*01  -COPY W414002      -L.                                               
008905     SKIP2                                                                
009005*01  -COPY W414003      -L.                                               
009105     SKIP2                                                                
009205*01  -COPY W414005      -L.                                               
009305     SKIP3                                                                
009405 FD  W41406                                                               
009505     RECORDING       V                                                    
009605     BLOCK CONTAINS  0.                                                   
009705     SKIP2                                                                
009805*01  SUA-POST -COPY W425SUA -PRE W41406- -L.                              
009905     SKIP2                                                                
010005*01  SUD-POST -COPY W425SUD -PRE W41406- -L.                              
010105     SKIP2                                                                
010205*01  SU1-POST -COPY W425SU1 -PRE W41406- -L.                              
010305     SKIP2                                                                
010405*01  SU2-POST -COPY W425SU2 -PRE W41406- -L.                              
010505     SKIP2                                                                
010605*01  SU3-POST -COPY W425SU3 -PRE W41406- -L.                              
010705     SKIP2                                                                
010805*01  SU4-POST -COPY W425SU4 -PRE W41406- -L.                              
010905     SKIP2                                                                
011005*01  SU5-POST -COPY W425SU5 -PRE W41406- -L.                              
011105     SKIP3                                                                
011205 FD  W41407                                                               
011305     RECORDING       F                                                    
011405     BLOCK CONTAINS  0.                                                   
011505     SKIP2                                                                
011605*01  POST -COPY W461001 -PRE  W41407-  -L.                                
011705     SKIP3                                                                
011805 FD  W41408                                                               
011905     RECORDING       F                                                    
012005     BLOCK CONTAINS  0.                                                   
012105     SKIP2                                                                
012205*01  POST -COPY W461015 -PRE  W41408-  -L.                                
012305     SKIP3                                                                
012405 FD  W41409                                                               
012505     RECORDING       F                                                    
012605     BLOCK CONTAINS  0.                                                   
012705     SKIP2                                                                
012805*01  POST -COPY W461020 -PRE  W41409-  -L.                                
012905     SKIP3                                                                
013005 FD  W4140A                                                               
013105     RECORDING       V                                                    
013205     BLOCK CONTAINS  0.                                                   
013305     SKIP2                                                                
013405*01  002-POST -COPY W461002 -PRE W4140A- -L.                              
013505     SKIP2                                                                
013605*01  003-POST -COPY W461003 -PRE W4140A- -L.                              
013705     SKIP2                                                                
013805*01  004-POST -COPY W461004 -PRE W4140A- -L.                              
013905     SKIP2                                                                
014005*01  005-POST -COPY W461005 -PRE W4140A- -L.                              
014105     SKIP2                                                                
014205*01  006-POST -COPY W461006 -PRE W4140A- -L.                              
014305     SKIP2                                                                
014405*01  007-POST -COPY W461007 -PRE W4140A- -L.                              
014505     SKIP2                                                                
014605*01  008-POST -COPY W461008 -PRE W4140A- -L.                              
014705     SKIP3                                                                
014805 FD  W4140J                                                               
014905     RECORDING       F                                                    
015005     BLOCK CONTAINS  0.                                                   
015105     SKIP2                                                                
015205*01  POST -COPY W414009  -PRE  W4140J-  -L.                               
015305     SKIP3                                                                
015405 FD  W4140D                                                               
015505     RECORDING       F                                                    
015605     BLOCK CONTAINS  0.                                                   
015705     SKIP2                                                                
015805*01  POST -COPY W414011 -PRE  W4140D-  -L.                                
015905     SKIP3                                                                
016005 FD  W4140E                                                               
016105     RECORDING       F                                                    
016205     BLOCK CONTAINS  0.                                                   
016305     SKIP2                                                                
016405*01  POST -COPY W414007 -PRE  W4140E-  -L.                                
016505     SKIP3                                                                
016605 FD  W4140G                                                               
016705     RECORDING       F                                                    
016805     BLOCK CONTAINS  0.                                                   
016905     SKIP2                                                                
017005*01  POST -COPY W461009 -PRE  W4140G-  -L.                                
017105     SKIP3                                                                
017205 FD  W41405                                                               
017305     RECORDING       V                                                    
017405     BLOCK CONTAINS  0.                                                   
017505     SKIP2                                                                
017605*01  -COPY W414006      -L.                                               
017705     SKIP3                                                                
017805 FD  W4140I                                                               
017905     RECORDING       F                                                    
018005     BLOCK CONTAINS  0.                                                   
018105     SKIP2                                                                
018205*01  POST -COPY W414010 -PRE  W4140I-  -L.                                
018305     SKIP3                                                                
018405 FD  W414X2                                                               
018505     RECORDING       V                                                    
018605     BLOCK CONTAINS  0.                                                   
018705     SKIP2                                                                
018805*01  POST -COPY W4140X2 -PRE  W414X2-  -L.                                
018905     SKIP3                                                                
019005 FD  W414X3                                                               
019105     RECORDING       V                                                    
019205     BLOCK CONTAINS  0.                                                   
019305     SKIP2                                                                
019405*01  POST -COPY W4140X3 -PRE  W414X3-  -L.                                
019505     SKIP3                                                                
019610 FD  W4140S                                                               
019712     RECORDING       F                                                    
019805     BLOCK CONTAINS  0.                                                   
019905     SKIP2                                                                
020010*01  POST -COPY W41403S -PRE  W4140S-  -L.                                
020105     EJECT                                                                
020205 WORKING-STORAGE SECTION.                                                 
020305     SKIP2                                                                
020405                                                                          
020505*    -- CHECKED BY WY2000                                                 
020605 77  IDPGM                       PIC X(8)    VALUE 'W4140300'.            
020705 77  JA                          PIC X       VALUE 'J'.                   
020805 77  NEJ                         PIC X       VALUE 'N'.                   
020905 77  W-TIAAVV                    PIC 9(4).                                
021205                                                                          
021305 77  W41401-EOF-SW               PIC X       VALUE 'N'.                   
021405     88  END-OF-W41401                       VALUE 'J'.                   
021505                                                                          
021605 77  W41403-EOF-SW               PIC X       VALUE 'N'.                   
021705     88  END-OF-W41403                       VALUE 'J'.                   
021805                                                                          
021905 77  W41405-EOF-SW               PIC X       VALUE 'N'.                   
022005     88  END-OF-W41405                       VALUE 'J'.                   
022105                                                                          
022205 01  WS-IDDISTR                PIC 9(4)      VALUE ZERO.                  
022305 01  WS-IDKUNDNR               PIC 9(6)      VALUE ZERO.                  
022405                                                                          
022505 01  TITPO-TIAAVV              PIC 9(4).                                  
022605 01  W-TIAVV REDEFINES TITPO-TIAAVV.                                      
022705     03  W-TIAVV-A             PIC 9(1).                                  
022805     03  W-TIAVV-AVV           PIC 9(3).                                  
022905 01  TITPO-TIAAP               PIC 9(3).                                  
023005     EJECT                                                                
023105 01      FILLER                PIC X(16) VALUE 'WS************'.          
023205*    --- ARBETSAREA FÖR BESTÄMNING AV LAGER                               
023305*      --- VALID IDDC CODES                                               
023405*                                                                         
023505*01    -COPY WWDC99                                                       
023605*01    -COPY WWDCKONS                                                     
023705       EJECT                                                              
023805                                                                          
023905*        -- REDIGERAS EN GÅNG PER OBKR & ORAD.                            
024005*           ANVÄNDS VID REDIG AV OLIKA POSTER.                            
024105 01      WS.                                                              
024205  02     WS-RED.                                                          
024305   03    WS-RED-KVBEART-OBK92    PIC S9(7)   VALUE ZERO  COMP-3.          
024405   03    WS-RED-KVBEART-Q-OBK92  PIC S9(7)   VALUE ZERO  COMP-3.          
024505   03    WS-RED-KDVRTPO          PIC 9(1)    VALUE ZERO.                  
024605   03    WS-RED-IDSUPPL-SU       PIC S9(5)   VALUE ZERO  COMP-3.          
024705   03    WS-RED-KDERS            PIC S9(3)   VALUE ZERO  COMP-3.          
024805                                                                          
024905*        -- OMVANDLING FÖR SU-POSTER                                      
025005  02     WS-SU-IDSUPPL-UT        PIC S9(5)      VALUE ZERO COMP-3.        
025105                                                                          
025205*        -- DATUM, GENERELLT HJÄLPFÄLT                                    
025305  02     WS-TIAAMMDD             PIC 9(6)       VALUE ZERO.               
025405  02     FILLER                  REDEFINES WS-TIAAMMDD.                   
025505   03    FILLER                  PIC X(4).                                
025605   03    WS-TIAAMMDD-DD          PIC 9(2).                                
025705                                                                          
025805*        -- DATUM + TIMMAR & MINUTER                                      
025905  02     WS-TIAAMMDDTTMM         PIC 9(10)      VALUE ZERO.               
026005  02     FILLER                  REDEFINES WS-TIAAMMDDTTMM.               
026105   03    WS-TIAAMMDDTTMM-AAMMDD  PIC 9(6).                                
026205   03    FILLER                  PIC X(4).                                
026305                                                                          
026405*        -- REDIG. FÖRE ANROP DE-                                         
026505  02     WS-KDRESTR              PIC S9(3)      VALUE ZERO COMP-3.        
026605     SKIP3                                                                
026705 01      FILLER                PIC X(16) VALUE 'K-KONSTANTER****'.        
026805 01      K-KONSTANTER.                                                    
026905                                                                          
027005*        -- IDKUNDRF MED "NOLL-VÄRDE"                                     
027105  02     K-IDKUNDRF-NOLL-LNG7    PIC X(10)   VALUE '0000000   '.          
027205     EJECT                                                                
027305 01  TEST-IDDISTR2               PIC 9(5)   COMP-3.                       
027405*01  FILLER  -COPY WWDIST98      -RED TEST-IDDISTR2.                      
027505     EJECT                                                                
027605 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
027705*01  FILLER  -COPY WWDIST20      -RED TEST-IDDISTR.                       
027805*01  FILLER  -COPY WWDIST24      -RED TEST-IDDISTR.                       
027905     EJECT                                                                
027906*    --- PARAMETRAR TILL WL10WBDC                                         
027907*01  -COPY WL10WBDC                                                       
027908     EJECT                                                                
028005 01  DYNAMISKA-SUBPROGRAM.                                                
028105*                                                                         
028205     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
028305     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
028405     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
028406     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
028505     SKIP2                                                                
028605*    --- PARAMETRAR TILL ABEND                                            
028705                                                                          
028805 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
028905 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
029005     SKIP2                                                                
029105 01  FELTEXT.                                                             
029205     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
029305     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
029405     EJECT                                                                
029505*    --- PARAMETRAR TILL POSTSUM                                          
029605*                                                                         
029705*01  -COPY W0005   -PRE  POSTSUM-                                         
029805     EJECT                                                                
029905*    --- PARAMETRAR TILL WDATKONV                                         
030005 01  FILLER                    PIC X(16) VALUE 'DAT-WDATAREA****'.        
030105     SKIP2                                                                
030205*01  -COPY WDATAREA                                                       
030305     EJECT                                                                
030405 01  W41401-AREA-START           PIC X(24)   VALUE                        
030505                                 'W41401-AREA-START  '.                   
030605     SKIP2                                                                
030705*01  AREA   -COPY W414001      -PRE W41401-                               
030805     EJECT                                                                
030905 01  W41403-AREA-START           PIC X(24)   VALUE                        
031005                                 'W41403-AREA-START  '.                   
031105     SKIP2                                                                
031205 01  W41403-AREA.                                                         
031305     03  W41403-IDPTYP           PIC X(3).                                
031405     03  FILLER                  PIC X(300).                              
031505*01  FILLER -COPY W414002      -PRE W41403-   -RED  W41403-AREA           
031605*01  FILLER -COPY W414003      -PRE W41403-   -RED  W41403-AREA           
031705*01  FILLER -COPY W414005      -PRE W41403-   -RED  W41403-AREA           
031805*01  FILLER -COPY W414011      -PRE W41403-   -RED  W41403-AREA           
031905     EJECT                                                                
032005 01  W41405-AREA-START           PIC X(24)   VALUE                        
032105                                 'W41405-AREA-START  '.                   
032205     SKIP2                                                                
032305 01  W41405-AREA.                                                         
032405     03  W41405-IDPTYP           PIC X(3).                                
032505     03  FILLER                  PIC X(300).                              
032605*01  FILLER -COPY W414006      -PRE W41405-   -RED  W41405-AREA           
032705     EJECT                                                                
032805 01  SUA-AREA-START              PIC X(16)   VALUE                        
032905                                 'SUA-AREA-START  '.                      
033005                                                                          
033105*01  AREA   -COPY W425SUA      -PRE SUA-                                  
033205     EJECT                                                                
033305 01  SUD-AREA-START              PIC X(16)   VALUE                        
033405                                 'SUD-AREA-START  '.                      
033505                                                                          
033605*01  AREA   -COPY W425SUD      -PRE SUD-                                  
033705     EJECT                                                                
033805 01  SU1-AREA-START              PIC X(16)   VALUE                        
033905                                 'SU1-AREA-START  '.                      
034005                                                                          
034105*01  AREA   -COPY W425SU1      -PRE SU1-                                  
034205     EJECT                                                                
034305 01  SU2-AREA-START              PIC X(16)   VALUE                        
034405                                 'SU2-AREA-START  '.                      
034505                                                                          
034605*01  AREA   -COPY W425SU2      -PRE SU2-                                  
034705     EJECT                                                                
034805 01  SU3-AREA-START              PIC X(16)   VALUE                        
034905                                 'SU3-AREA-START  '.                      
035005                                                                          
035105*01  AREA   -COPY W425SU3      -PRE SU3-                                  
035205     EJECT                                                                
035305 01  SU4-AREA-START              PIC X(16)   VALUE                        
035405                                 'SU4-AREA-START  '.                      
035505                                                                          
035605*01  AREA   -COPY W425SU4      -PRE SU4-                                  
035705     EJECT                                                                
035805 01  SU5-AREA-START              PIC X(16)   VALUE                        
035905                                 'SU5-AREA-START  '.                      
036005                                                                          
036105*01  AREA   -COPY W425SU5      -PRE SU5-                                  
036205     EJECT                                                                
036305 01  W41407-AREA-START           PIC X(24)   VALUE                        
036405                                 'W41407-AREA-START  '.                   
036505     SKIP2                                                                
036605                                                                          
036705*01  AREA -COPY W461001     -PRE W41407-                                  
036805     EJECT                                                                
036905 01  W41408-AREA-START           PIC X(24)   VALUE                        
037005                                 'W41408-AREA-START  '.                   
037105     SKIP2                                                                
037205                                                                          
037305*01  AREA -COPY W461015     -PRE W41408-                                  
037405     EJECT                                                                
037505 01  W41409-AREA-START           PIC X(24)   VALUE                        
037605                                 'W41409-AREA-START  '.                   
037705     SKIP2                                                                
037805                                                                          
037905*01  AREA -COPY W461020     -PRE W41409-                                  
038005                                                                          
038105     EJECT                                                                
038205 01  FILLER                      PIC X(16)   VALUE                        
038305                                 'COPY W461002'.                          
038405                                                                          
038505*01  FILLER -COPY W461002      -PRE W4140A-                               
038605     EJECT                                                                
038705 01  FILLER                      PIC X(16)   VALUE                        
038805                                 'COPY W461003'.                          
038905                                                                          
039005*01  FILLER -COPY W461003      -PRE W4140A-                               
039105     EJECT                                                                
039205 01  FILLER                      PIC X(16)   VALUE                        
039305                                 'COPY W461004'.                          
039405                                                                          
039505*01  FILLER -COPY W461004      -PRE W4140A-                               
039605     EJECT                                                                
039705 01  FILLER                      PIC X(16)   VALUE                        
039805                                 'COPY W461005'.                          
039905                                                                          
040005*01  FILLER -COPY W461005      -PRE W4140A-                               
040105     EJECT                                                                
040205 01  FILLER                      PIC X(16)   VALUE                        
040305                                 'COPY W461006'.                          
040405                                                                          
040505*01  FILLER -COPY W461006      -PRE W4140A-                               
040605     EJECT                                                                
040705 01  FILLER                      PIC X(16)   VALUE                        
040805                                 'COPY W461007'.                          
040905                                                                          
041005*01  FILLER -COPY W461007      -PRE W4140A-                               
041105     EJECT                                                                
041205 01  FILLER                      PIC X(16)   VALUE                        
041305                                 'COPY W461008'.                          
041405                                                                          
041505*01  FILLER -COPY W461008      -PRE W4140A-                               
041605     EJECT                                                                
041705 01  W4140J-AREA-START           PIC X(24)   VALUE                        
041805                                 'W4140J-AREA-START  '.                   
041905     SKIP2                                                                
042005*01  AREA -COPY W414009     -PRE W4140J-                                  
042105     EJECT                                                                
042205 01  ANNVOR-AREA-START           PIC X(24)   VALUE                        
042305                                 'ANNVOR-AREA-START  '.                   
042405     SKIP2                                                                
042505                                                                          
042605*01  ANNVOR-AREA -COPY W414011                                            
042705     EJECT                                                                
042805 01  W4140E-AREA-START           PIC X(24)   VALUE                        
042905                                 'W4140E-AREA-START  '.                   
043005     SKIP2                                                                
043105                                                                          
043205*01  AREA -COPY W414007     -PRE W4140E-                                  
043305     EJECT                                                                
043405 01  W4140G-AREA-START           PIC X(24)   VALUE                        
043505                                 'W4140G-AREA-START  '.                   
043605     SKIP2                                                                
043705                                                                          
043805*01  AREA -COPY W461009     -PRE W4140G-                                  
043905     EJECT                                                                
044005 01  W4140I-AREA-START           PIC X(24)   VALUE                        
044105                                 'W4140I-AREA-START  '.                   
044205     SKIP2                                                                
044305                                                                          
044405*01  AREA -COPY W414010     -PRE W4140I-                                  
044505                                                                          
044605     EJECT                                                                
044705 01  W414X2-AREA-START           PIC X(24)   VALUE                        
044805                                 'W414X2-AREA-START  '.                   
044905                                                                          
045005*01  AREA -COPY W4140X2     -PRE W414X2-                                  
045105                                                                          
045205     EJECT                                                                
045305 01  W414X3-AREA-START           PIC X(24)   VALUE                        
045405                                 'W414X3-AREA-START  '.                   
045505                                                                          
045605*01  AREA -COPY W4140X3     -PRE W414X3-                                  
045705                                                                          
045905     EJECT                                                                
046011 01  W4140S-AREA-START           PIC X(24)   VALUE                        
046111                                 'W4140S-AREA-START  '.                   
046205                                                                          
046311*01  AREA -COPY W41403S     -PRE W4140S-                                  
046405                                                                          
046505     EJECT                                                                
046605 PROCEDURE DIVISION.                                                      
046705     SKIP2                                                                
046805     PERFORM A-INIT                                                       
046905                                                                          
047005     PERFORM S01-LAES-W41401                                              
047105     PERFORM UNTIL (END-OF-W41401)                                        
047205       PERFORM B-BEH-OHUV                                                 
047305       PERFORM S01-LAES-W41401                                            
047405     END-PERFORM                                                          
047505                                                                          
047605     PERFORM S02-LAES-W41403                                              
047705     PERFORM UNTIL (END-OF-W41403)                                        
047805       IF  W41403-IDPTYP = '201'                                          
047905         PERFORM C-BEH-ORAD                                               
048005       END-IF                                                             
048105       IF W41403-IDPTYP = '202'                                           
048205         PERFORM D-BEH-OBKR                                               
048305       END-IF                                                             
048405       IF W41403-IDPTYP = '203'                                           
048505         PERFORM E-BEH-TILLTPO                                            
048605       END-IF                                                             
048705       IF W41403-IDPTYP = '205'                                           
048805         PERFORM G-BEH-ANNVOR                                             
048905       END-IF                                                             
049005       PERFORM S02-LAES-W41403                                            
049105     END-PERFORM                                                          
049205                                                                          
049305     PERFORM S03-LAES-W41405                                              
049405     PERFORM UNTIL (END-OF-W41405)                                        
049505         PERFORM F-BEH-REGTPO                                             
049605       PERFORM S03-LAES-W41405                                            
049705     END-PERFORM                                                          
049805                                                                          
049905     PERFORM Z-FINIT                                                      
050005                                                                          
050105     MOVE ZERO TO RETURN-CODE                                             
050205     GOBACK                                                               
050305     .                                                                    
050405     EJECT                                                                
050505 A-INIT SECTION.                                                          
050605                                                                          
050705     OPEN INPUT  W41401                                                   
050805                 W41403                                                   
050905                 W41405                                                   
051005                                                                          
051105     OPEN OUTPUT W41406                                                   
051205                 W41407                                                   
051305                 W41408                                                   
051405                 W41409                                                   
051505                 W4140A                                                   
051605                 W4140J                                                   
051705                 W4140D                                                   
051805                 W4140E                                                   
051905                 W4140G                                                   
052005                 W4140I                                                   
052105                 W414X2 W414X3                                            
052212                 W4140S                                                   
052305                                                                          
052405     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
052505     .                                                                    
052605     EJECT                                                                
052705 B-BEH-OHUV SECTION.                                                      
052805                                                                          
052905     PERFORM BB-NOAC-002-OBHUV                                            
053005     .                                                                    
053105     EJECT                                                                
053205 BB-NOAC-002-OBHUV SECTION.                                               
053305                                                                          
053405     MOVE SPACE                  TO W4140A-OBHUV-W461002                  
053505                                                                          
053605     MOVE '002'                  TO W4140A-OBHUV-IDPTYP                   
053705                                                                          
053805     MOVE W41401-OHUV-IDDISTR    TO W4140A-OBHUV-IDDISTR                  
053905     MOVE W41401-OHUV-IDKUNDNR   TO W4140A-OBHUV-IDKUNDNR                 
054005     MOVE W41401-OHUV-KDFRAKT    TO W4140A-OBHUV-KDFRAKT                  
054105     MOVE W41401-OHUV-IDKUNDRF (1:7) TO W4140A-OBHUV-IDORDNR              
054205     MOVE W41401-OHUV-BEKUNDRF   TO W4140A-OBHUV-BEVOLREF                 
054305     MOVE W41401-OHUV-BEVARREF   TO W4140A-OBHUV-BEVARREF                 
054405     MOVE W41401-OHUV-KDORDKL    TO W4140A-OBHUV-KDORDKL                  
054505     MOVE W41401-OHUV-TIREGDAT-ORDER TO W4140A-OBHUV-TIORDREG             
054605     MOVE W41401-OHUV-KDFAKTYP   TO W4140A-OBHUV-KDFAKTYP                 
054705                                                                          
054805     PERFORM S22-SKRIV-W4140A-002                                         
054905     .                                                                    
055005     EJECT                                                                
055105 C-BEH-ORAD SECTION.                                                      
055205                                                                          
055305     PERFORM CX-INIT-ORAD                                                 
055405                                                                          
055505     IF  W41403-ORAD-PRARTNTO < W41403-ORAD-PRARTSJK                      
055605     OR  W41403-ORAD-PRARTNTO = ZERO                                      
055705     OR  W41403-ORAD-PRARTSJK * 2.0 = W41403-ORAD-PRARTNTO                
055805       PERFORM CA-PRISLARM                                                
055905     END-IF                                                               
056005                                                                          
056105     IF  W41403-ORAD-IDKUNDRF-RO = K-IDKUNDRF-NOLL-LNG7                   
056205                                                                          
056305       IF  W41403-ORAD-KVBEART > ZERO                                     
056405       AND W41403-ORAD-FLTILLK = NEJ                                      
056505                                                                          
056605         IF  W41403-ORAD-IDSYSTEM NOT = 'VR'                              
056705             IF W41403-ORAD-FLOVRLEV = NEJ AND (NOT DIST24-NORGE)         
056805                PERFORM CB-VR-SU1                                         
056905             END-IF                                                       
057005         END-IF                                                           
057105                                                                          
057205         MOVE W41403-ORAD-IDDISTR TO TEST-IDDISTR2                        
057305                                                                          
057405         IF  W41403-ORAD-IDSYSTEM NOT = 'VIPS'                            
057505         AND W41403-ORAD-IDSYSTEM NOT = 'VDI'                             
057605           IF  DIST98-EJ-ONORDER                                          
057705           AND W41403-ORAD-IDSYSTEM   = 'LDC'                             
057805             CONTINUE                                                     
057905           ELSE                                                           
058005             IF W41403-ORAD-FLOVRLEV = NEJ                                
058105                PERFORM CC-NOAC-015-BYPASS                                
058205                PERFORM CD-NOAC-020-ONORD                                 
058305             END-IF                                                       
058405           END-IF                                                         
058505         END-IF                                                           
058605       END-IF                                                             
058705     END-IF                                                               
058805                                                                          
058905     IF W41403-ORAD-IDSYSTEM = 'SPX'                                      
059005       PERFORM CE-SPX-0X2-ORAD                                            
059105     END-IF                                                               
059205                                                                          
059312     IF W41403-ORAD-BERADREF = 'REFSCRAP  ' OR                            
059412                               'INHSCRAP  ' OR                            
059512                               'RETSCRAP  ' OR                            
059612                               'QALSCRAP  ' OR                            
059712                               'ECOSCRAP  ' OR                            
059812                               'MIXSCRAP  '                               
059905       PERFORM CF-SKRIV-SKROTFIL                                          
060005     END-IF                                                               
060105     .                                                                    
060205     EJECT                                                                
060305 CA-PRISLARM SECTION.                                                     
060405                                                                          
060505     MOVE SPACE                  TO W4140J-AREA                           
060605                                                                          
060705     MOVE 'AMC'                  TO W4140J-AMC-IDPTYP                     
060805     MOVE W41403-ORAD-IDDISTR    TO W4140J-AMC-IDDISTR                    
060905     MOVE W41403-ORAD-IDKUNDNR   TO W4140J-AMC-IDKUNDNR                   
061005     MOVE W41403-ORAD-IDORDER    TO W4140J-AMC-IDORDER                    
061105     MOVE W41403-ORAD-KDORDKL    TO W4140J-AMC-KDORDKL                    
061205     MOVE W41403-ORAD-KDFAKTYP   TO W4140J-AMC-KDFAKTYP                   
061305     MOVE W41403-ORAD-IDARTNR    TO W4140J-AMC-IDARTNR                    
061405     MOVE W41403-ORAD-KDPRODSL   TO W4140J-AMC-KDPRODSL                   
061505     MOVE W41403-ORAD-IDFKNGRP   TO W4140J-AMC-IDFKNGRP                   
061605     MOVE W41403-ORAD-PRARTNTO   TO W4140J-AMC-PRARTNTO                   
061705     MOVE W41403-ORAD-KVBEART    TO W4140J-AMC-KVBEART                    
061805     MOVE W41403-ORAD-PRARTSJK   TO W4140J-AMC-PRARTSJK                   
061905     MOVE W41403-ORAD-TIREGDAT-ORDER TO W4140J-AMC-TIREGDAT               
062005     MOVE W41403-ORAD-TIRODAT    TO W4140J-AMC-TIRODAT                    
062105     MOVE W41403-ORAD-PRBPRIS    TO W4140J-AMC-PRBPRIS                    
062205                                                                          
062305     IF  W41403-ORAD-PRARTNTO < W41403-ORAD-PRARTSJK                      
062405         MOVE '001'              TO W4140J-AMC-IDFELKOD                   
062505     ELSE                                                                 
062605       IF  W41403-ORAD-PRARTNTO  = W41403-ORAD-PRARTSJK                   
062705         MOVE '002'              TO W4140J-AMC-IDFELKOD                   
062805       ELSE                                                               
062905         MOVE '003'              TO W4140J-AMC-IDFELKOD                   
063005       END-IF                                                             
063105     END-IF                                                               
063205                                                                          
063305                                                                          
063405     PERFORM S32-SKRIV-W4140J                                             
063505     .                                                                    
063605     EJECT                                                                
063705 CB-VR-SU1 SECTION.                                                       
063805                                                                          
063905     MOVE SPACE                  TO SU1-AREA                              
064005                                                                          
064105     MOVE 'SU1'                  TO SU1-IDPTYP                            
064205                                                                          
064305     MOVE W41403-ORAD-IDDISTR    TO WS-IDDISTR                            
064405     MOVE WS-IDDISTR             TO SU1-IDDISTR                           
064505     MOVE W41403-ORAD-IDKUNDNR   TO WS-IDKUNDNR                           
064605     MOVE WS-IDKUNDNR            TO SU1-IDKUNDNR                          
064705     MOVE WS-RED-IDSUPPL-SU      TO SU1-IDSUPPL                           
064805                                                                          
064905     MOVE W41403-ORAD-IDKUNDRF(1:7) TO SU1-IDORDNR-002                    
065005                                                                          
065105     MOVE W41403-ORAD-IDARTNR    TO SU1-IDARTNR                           
065205     MOVE W41403-ORAD-REKSIFFR   TO SU1-REKSIFFR                          
065305     MOVE W41403-ORAD-KVBEART    TO SU1-KVBEART-002                       
065405     MOVE W41403-ORAD-KDORDKL    TO SU1-KDORDER                           
065505     MOVE W41403-ORAD-KDTPOTYP   TO SU1-KDTPOTYP                          
065605     MOVE W41403-ORAD-KDFAKTYP   TO SU1-KDFAKTYP                          
065705                                                                          
065805     IF  W41403-ORAD-KDPRTYP = 'P'                                        
065905       MOVE 'M'                  TO SU1-KDMANPR                           
066005     ELSE                                                                 
066105       MOVE W41403-ORAD-FLPRTILL TO SU1-KDMANPR                           
066205     END-IF                                                               
066305                                                                          
066405     MOVE WS-RED-KDVRTPO         TO SU1-KDVRTPO                           
066505                                                                          
066605     MOVE W41403-ORAD-KDVRINFO   TO SU1-KDVRINFO                          
066705                                                                          
066805     MOVE W41403-ORAD-TIREGDAT   TO SU1-TIAAMMDD-REG                      
066905     MOVE W41403-ORAD-TIKLOCK    TO SU1-TIKLOCK-REG                       
067005                                                                          
067105     PERFORM S13-SKRIV-W41406-SU1                                         
067205     .                                                                    
067305     EJECT                                                                
067405 CC-NOAC-015-BYPASS SECTION.                                              
067505                                                                          
067605     MOVE SPACE                  TO W41408-AREA                           
067705                                                                          
067805     MOVE '015'                  TO W41408-BYPASS-IDPTYP                  
067905                                                                          
068005     MOVE W41403-ORAD-IDDISTR    TO W41408-BYPASS-IDDISTR                 
068105     MOVE W41403-ORAD-IDKUNDNR   TO W41408-BYPASS-IDKUNDNR                
068205                                                                          
068305     MOVE W41403-ORAD-IDKUNDRF (1:7) TO W41408-BYPASS-IDORDNR             
068405                                                                          
068505     MOVE W41403-ORAD-KDORDKL    TO W41408-BYPASS-KDORDKL                 
068605     MOVE W41403-ORAD-IDARTNR    TO W41408-BYPASS-IDARTNR                 
068705     MOVE W41403-ORAD-REKSIFFR   TO W41408-BYPASS-REKSIFFR                
068805     MOVE W41403-ORAD-BERADREF   TO W41408-BYPASS-BERADREF                
068905     MOVE W41403-ORAD-BEVOLREF   TO W41408-BYPASS-BEVOLREF                
069005     MOVE W41403-ORAD-KVBEART    TO W41408-BYPASS-KVBEART                 
069105     MOVE W41403-ORAD-KDFAKTYP   TO W41408-BYPASS-KDFAKTYP                
069205     MOVE W41403-ORAD-KDDSP      TO W41408-BYPASS-KDDSP                   
069305     MOVE 'N'                    TO W41408-BYPASS-FLABON                  
069405     MOVE W41403-ORAD-KDTPOTYP   TO W41408-BYPASS-KDTPOTYP                
069505                                                                          
069605     PERFORM S18-SKRIV-W41408                                             
069705     .                                                                    
069805     EJECT                                                                
069905 CD-NOAC-020-ONORD SECTION.                                               
070005                                                                          
070105     MOVE SPACE                  TO W41409-AREA                           
070205                                                                          
070305     MOVE '020'                  TO W41409-ONORD-IDPTYP                   
070405                                                                          
070505     MOVE W41403-ORAD-IDDISTR    TO W41409-ONORD-IDDISTR                  
070605     MOVE W41403-ORAD-IDKUNDNR   TO W41409-ONORD-IDKUNDNR                 
070705                                                                          
070805     MOVE W41403-ORAD-IDKUNDRF (1:7) TO W41409-ONORD-IDORDNR              
070905                                                                          
071005     MOVE W41403-ORAD-IDDC       TO W41409-ONORD-IDDC                     
071105     MOVE W41403-ORAD-KDORDKL    TO W41409-ONORD-KDORDKL                  
071205     MOVE W41403-ORAD-BEVARREF   TO W41409-ONORD-BEVARREF                 
071305     MOVE W41403-ORAD-BEVOLREF   TO W41409-ONORD-BEVOLREF                 
071405     MOVE W41403-ORAD-BERADREF   TO W41409-ONORD-BERADREF                 
071505     MOVE W41403-ORAD-IDARTNR    TO W41409-ONORD-IDARTNR                  
071605     MOVE W41403-ORAD-REKSIFFR   TO W41409-ONORD-REKSIFFR                 
071705     MOVE W41403-ORAD-KVBEART    TO W41409-ONORD-KVBEART                  
071805     MOVE W41403-ORAD-TIREGDAT-ORDER TO W41409-ONORD-TIORDREG             
071905     MOVE ZERO                   TO W41409-ONORD-IDRONR                   
072005     MOVE ZERO                   TO W41409-ONORD-TIRODAT                  
072105     MOVE W41403-ORAD-PRARTNTO   TO W41409-ONORD-PRARTNTO                 
072205     MOVE W41403-ORAD-PRARTBTO-EXP TO W41409-ONORD-PRARTBTO-EXP           
072305     MOVE W41403-ORAD-KDPRODSL   TO W41409-ONORD-KDPRODSL                 
072405     MOVE W41403-ORAD-IDFKNGRP   TO W41409-ONORD-IDFKNGRP                 
072505     MOVE W41403-ORAD-FLINVEST   TO W41409-ONORD-FLINVEST                 
072605     MOVE W41403-ORAD-KDDSP      TO W41409-ONORD-KDDSP                    
072705                                                                          
072805     IF  W41403-ORAD-KDPRTYP = 'P'                                        
072905       MOVE 'M'                  TO W41409-ONORD-KDMANPR                  
073005     ELSE                                                                 
073105       MOVE W41403-ORAD-FLPRTILL TO W41409-ONORD-KDMANPR                  
073205     END-IF                                                               
073305                                                                          
073405     MOVE 'N'                    TO W41409-ONORD-FLABON                   
073505     MOVE W41403-ORAD-KDTPOTYP   TO W41409-ONORD-KDTPOTYP                 
073605                                                                          
073705     PERFORM S19-SKRIV-W41409                                             
073805     .                                                                    
073905     EJECT                                                                
074005 CE-SPX-0X2-ORAD   SECTION.                                               
074105                                                                          
074205     MOVE SPACE                  TO W414X2-AREA                           
074305                                                                          
074405     MOVE '0X2'                  TO W414X2-0X2-IDPTYP                     
074505     MOVE W41403-ORAD-IDSYSTEM   TO W414X2-0X2-IDSYSTEM                   
074605     MOVE W41403-ORAD-IDDISTR    TO W414X2-0X2-IDDISTR                    
074705     MOVE W41403-ORAD-IDKUNDNR   TO W414X2-0X2-IDKUNDNR                   
074805     MOVE W41403-ORAD-IDKUNDRF   TO W414X2-0X2-IDKUNDRF                   
074905     MOVE W41403-ORAD-IDDC       TO W414X2-0X2-IDDC                       
075005     MOVE W41403-ORAD-IDARTNR    TO W414X2-0X2-IDARTNR                    
075105     MOVE W41403-ORAD-KVBEART    TO W414X2-0X2-KVBEART                    
075205     MOVE W41403-ORAD-KVBEART-Q  TO W414X2-0X2-KVBEART-Q                  
075305     MOVE W41403-ORAD-BERADREF   TO W414X2-0X2-BERADREF                   
075405                                                                          
075505     PERFORM S40-SKRIV-W414X2                                             
075605     .                                                                    
075705     EJECT                                                                
075805 CF-SKRIV-SKROTFIL SECTION.                                               
075905                                                                          
076010     MOVE 'SCR'                  TO W4140S-IDPTYP                         
076112                                                                          
076312     IF W41403-ORAD-BERADREF = 'REFSCRAP  '                               
076412       MOVE 1                    TO W4140S-KDSORT1                        
076512     END-IF                                                               
076612     IF W41403-ORAD-BERADREF = 'INHSCRAP  '                               
076712       MOVE 2                    TO W4140S-KDSORT1                        
076812     END-IF                                                               
076912     IF W41403-ORAD-BERADREF = 'RETSCRAP  '                               
077012       MOVE 3                    TO W4140S-KDSORT1                        
077112     END-IF                                                               
077212     IF W41403-ORAD-BERADREF = 'QALSCRAP  '                               
077312       MOVE 4                    TO W4140S-KDSORT1                        
077412     END-IF                                                               
077512     IF W41403-ORAD-BERADREF = 'ECOSCRAP  '                               
077613       MOVE 5                    TO W4140S-KDSORT1                        
077712     END-IF                                                               
077812     IF W41403-ORAD-BERADREF = 'MIXSCRAP  '                               
077912       MOVE 6                    TO W4140S-KDSORT1                        
078012     END-IF                                                               
078112     MOVE W41403-ORAD-BERADREF   TO W4140S-BERADREF                       
078312                                                                          
079308     MOVE W41403-ORAD-IDDC       TO WBDC-IDDC                             
079309     CALL WL10WBDC USING WBDC-AREA                                        
079310     IF WBDC-FLWEBDC = JA                                                 
079320       MOVE WBDC-KDMFUP          TO W4140S-KDMFUP                         
079330     ELSE                                                                 
079340       MOVE SPACE                TO W4140S-KDMFUP                         
079350     END-IF                                                               
079360                                                                          
079414     MOVE W41403-ORAD-IDDC       TO W4140S-IDDC                           
079510     MOVE W41403-ORAD-IDDISTR    TO W4140S-IDDISTR                        
079610     MOVE W41403-ORAD-IDKUNDNR   TO W4140S-IDKUNDNR                       
079710     MOVE W41403-ORAD-IDKUNDRF   TO W4140S-IDKUNDRF                       
079810     MOVE W41403-ORAD-IDARTNR    TO W4140S-IDARTNR                        
079910     MOVE W41403-ORAD-KVBEART    TO W4140S-KVBEART                        
080010     MOVE W41403-ORAD-KVBEART-Q  TO W4140S-KVBEART-Q                      
080110     MOVE W41403-ORAD-PRARTNTO   TO W4140S-PRARTNTO                       
080207                                                                          
080310     COMPUTE W4140S-SUARTNTO = W41403-ORAD-PRARTNTO *                     
080407                               W41403-ORAD-KVBEART                        
080512                                                                          
080612     PERFORM S42-SKRIV-W4140S                                             
080807     .                                                                    
080907     EJECT                                                                
081007 CX-INIT-ORAD SECTION.                                                    
081107                                                                          
081207*    -- REDIGERA IDDISTR & IDSUPPL MHT FTAG & KUNDSTRUKTUR                
081307     MOVE W41403-ORAD-IDDC       TO WS-IDDC                               
081407     PERFORM S51-RED-IDSUPPL-SU                                           
081507     MOVE WS-SU-IDSUPPL-UT       TO WS-RED-IDSUPPL-SU                     
081607                                                                          
081707*    -- REDIGERA KDVRTPO MHT KDTPOTYP & IDSYSTEM                          
081807     IF  W41403-ORAD-KDTPOTYP = +1                                        
081907       IF  W41403-ORAD-IDSYSTEM = 'VR  '                                  
082007         MOVE +1                 TO WS-RED-KDVRTPO                        
082107       ELSE                                                               
082207         MOVE +2                 TO WS-RED-KDVRTPO                        
082307       END-IF                                                             
082407     ELSE                                                                 
082507       MOVE +0                   TO WS-RED-KDVRTPO                        
082607     END-IF                                                               
082707     .                                                                    
082807     EJECT                                                                
082907 D-BEH-OBKR SECTION.                                                      
083007                                                                          
083107     PERFORM DX-INIT-OBKR                                                 
083207                                                                          
083307     IF  W41403-OBKR-IDARTNR-TILLK = ZERO                                 
083407     AND W41403-OBKR-BEERS      =  SPACE                                  
083408       IF  W41403-OBKR-KDORDBEK =  41 OR 61                               
083409                                OR 51 OR 52                               
083410                                OR 53 OR 54 OR 55 OR 57 OR 66             
083420                                OR 67 OR 70 OR 75 OR 76 OR 80             
083807         IF  W41403-OBKR-KDTPOTYP = ZERO                                  
083907         OR  (W41403-OBKR-KDTPOTYP = +6                                   
084007          AND W41403-OBKR-KDORDBEK = 70)                                  
084107                                                                          
084207           PERFORM DA-DEP-IDSYSTEM                                        
084307         END-IF                                                           
084407       END-IF                                                             
084507     END-IF                                                               
084607                                                                          
084707     IF  W41403-OBKR-KDORDBEK = 92                                        
084807       IF W41403-OBKR-KVPREAVB = ZERO                                     
084907         PERFORM DA-DEP-IDSYSTEM                                          
085007       END-IF                                                             
085107       PERFORM DB-SRS-VOR                                                 
085207     END-IF                                                               
085307                                                                          
085407     IF  W41403-OBKR-KDTPOTYP   =  ZERO                                   
085408       IF  W41403-OBKR-KDORDBEK =  43 OR 44                               
085409                                OR 51 OR 52 OR 53 OR 54                   
085410                                OR 55 OR 57 OR 58 OR 59 OR 66             
085420                                OR 67 OR 75 OR 76 OR 80 OR 92             
085430                                                                          
085907         IF  W41403-OBKR-KDORDBEK =  43 OR 44                             
086007           PERFORM DG-NOAC-006-OBKVAN                                     
086107         END-IF                                                           
086207                                                                          
086307         IF  W41403-OBKR-KDORDBEK  =  51 OR 53 OR 54 OR 55 OR 57          
086407                                   OR 58 OR 59 OR 66 OR 67                
086507                                   OR 75 OR 76 OR 52                      
086607           PERFORM DH-NOAC-008-OBSTOP-BEST                                
086707         END-IF                                                           
086807                                                                          
086907         IF  W41403-OBKR-KDORDBEK =  54 AND (NOT DIST24-NORGE)            
087007           PERFORM DI-VR-SU2-UTG                                          
087107         END-IF                                                           
087207                                                                          
087208         IF  W41403-OBKR-KDORDBEK =  43 OR 44 OR 51 OR 53 OR 52           
087209                                  OR 55 OR 57 OR 58 OR 59 OR 66           
087210                                  OR 67 OR 75 OR 76 OR 80 OR 92           
087220                    AND (NOT DIST24-NORGE)                                
087707           PERFORM DJ-VR-SU3                                              
087807         END-IF                                                           
087907                                                                          
088007         IF  W41403-OBKR-KDORDBEK =  80                                   
088107           PERFORM DK-NOAC-009-SERV                                       
088207         END-IF                                                           
088307                                                                          
088407         IF  W41403-OBKR-KDORDBEK =  80 OR 92                             
088507           PERFORM DL-NOAC-007-OBLAG                                      
088607         END-IF                                                           
088707                                                                          
088807       END-IF                                                             
088907     END-IF                                                               
089007                                                                          
089107     IF  W41403-OBKR-KDORDBEK = 10                                        
089207       MOVE W41403-OBKR-KDORDBEK TO WS-KDRESTR                            
089307       PERFORM DE-NOAC-001-BIP                                            
089407       IF NOT DIST24-NORGE                                                
089507         PERFORM DF-VR-SUA                                                
089607       END-IF                                                             
089707     END-IF                                                               
089807                                                                          
089907     IF  W41403-OBKR-KDORDBEK =  61                                       
090007       IF  W41403-OBKR-BEERS = SPACE                                      
090107       AND W41403-OBKR-IDARTNR-TILLK = ZERO                               
090207                                                                          
090307         IF  W41403-OBKR-KDORDBEK  =  61                                  
090407         AND W41403-OBKR-KDERS     =  ZERO                                
090507           PERFORM DH-NOAC-008-OBSTOP-BEST                                
090607         END-IF                                                           
090707                                                                          
090807         IF NOT DIST24-NORGE                                              
090907           PERFORM DJ-VR-SU3                                              
091007         END-IF                                                           
091107       END-IF                                                             
091207     END-IF                                                               
091307                                                                          
091407     IF  W41403-OBKR-IDARTNR-TILLK > ZERO                                 
091507                                                                          
091607       IF  W41403-OBKR-KDORDBEK =  41                                     
091707         IF NOT DIST24-NORGE                                              
091807           PERFORM DN-VR-SU2-TILLK                                        
091907         END-IF                                                           
092007         PERFORM DO-NOAC-003-OBEN                                         
092107       END-IF                                                             
092207                                                                          
092307       IF  W41403-OBKR-KDORDBEK =  61                                     
092407         PERFORM DP-NOAC-004-OBEEN                                        
092507         PERFORM DQ-NOAC-008-OBSTOP-TILLK                                 
092607       END-IF                                                             
092707     END-IF                                                               
092807                                                                          
092907     IF  W41403-OBKR-BEERS NOT = SPACE                                    
093007       PERFORM DR-NOAC-005-OBTEXT                                         
093107     END-IF                                                               
093207                                                                          
093307     IF  W41403-OBKR-KDORDBEK NOT = 10                                    
093407       IF  W41403-OBKR-KDTPOTYP > ZERO                                    
093507         IF  W41403-OBKR-KDORDBEK =  72 OR 73 OR 75 OR 76                 
093607         OR  (W41403-OBKR-KDORDBEK = 74                                   
093707          AND W41403-OBKR-KDTPOTYP = +1)                                  
093807           IF NOT DIST24-NORGE                                            
093907             PERFORM DS-VR-SUD                                            
094007           END-IF                                                         
094107         ELSE                                                             
094207           IF  W41403-OBKR-IDARTNR-TILLK = ZERO                           
094307           AND W41403-OBKR-BEERS = SPACE                                  
094407                                                                          
094507             IF  (W41403-OBKR-KDTPOTYP = 1 OR 2 OR 3 OR 4 OR 5)           
094607                                                                          
094707               IF  (W41403-OBKR-KDTPOTYP = 2                              
094807                AND W41403-OBKR-KDORDBEK = 70)                            
094907               OR  W41403-OBKR-KDORDBEK = 85                              
095007               OR  W41403-OBKR-KDORDBEK = 71 OR 77                        
095107                 CONTINUE                                                 
095207               ELSE                                                       
095307                 IF NOT DIST24-NORGE                                      
095407                   PERFORM DS-VR-SUD                                      
095507                 END-IF                                                   
095607               END-IF                                                     
095707             END-IF                                                       
095807           END-IF                                                         
095907         END-IF                                                           
096007       END-IF                                                             
096107     END-IF                                                               
096207                                                                          
096307     IF W41403-OBKR-KDTPOTYP = 3                                          
096407       IF W41403-OBKR-KDORDBEK = 52 OR 53 OR 54 OR 55 OR 58 OR            
096507         59 OR 66 OR 67 OR 72 OR 73 OR 74 OR 75 OR 76 OR 80 OR            
096607         81 OR 82 OR 85 OR 41 OR 61 OR 57 OR 51                           
096707         IF  W41403-OBKR-IDARTNR-TILLK = ZERO                             
096807             AND W41403-OBKR-BEERS = SPACE                                
096907             PERFORM DT-BASLAGER                                          
097007         END-IF                                                           
097107       END-IF                                                             
097207     END-IF                                                               
097307                                                                          
097407     IF W41403-OBKR-IDSYSTEM = 'SPX'                                      
097507       PERFORM DU-SPX-0X3-OBKR                                            
097607     END-IF                                                               
097707     .                                                                    
097807     EJECT                                                                
097907 DA-DEP-IDSYSTEM SECTION.                                                 
098007                                                                          
098107     IF  W41403-OBKR-IDSYSTEM NOT = 'VR'                                  
098207         IF W41403-OBKR-FLOVRLEV = NEJ AND (NOT DIST24-NORGE)             
098307            PERFORM DAA-VR-SU1                                            
098407         END-IF                                                           
098507     END-IF                                                               
098607                                                                          
098707     MOVE W41403-OBKR-IDDISTR TO TEST-IDDISTR2                            
098807                                                                          
098907     IF  W41403-OBKR-IDSYSTEM NOT = 'VIPS'                                
099007     AND W41403-OBKR-IDSYSTEM NOT = 'VDI'                                 
099107       IF  DIST98-EJ-ONORDER                                              
099207       AND W41403-OBKR-IDSYSTEM   = 'LDC'                                 
099307         CONTINUE                                                         
099407       ELSE                                                               
099507         IF W41403-OBKR-FLOVRLEV = NEJ                                    
099607            PERFORM DAB-NOAC-015-BYPASS                                   
099707            PERFORM DAC-NOAC-020-ONORD                                    
099807         END-IF                                                           
099907       END-IF                                                             
100007     END-IF                                                               
100107     .                                                                    
100207     EJECT                                                                
100307 DAA-VR-SU1 SECTION.                                                      
100407                                                                          
100507     MOVE SPACE                  TO SU1-AREA                              
100607                                                                          
100707     MOVE 'SU1'                  TO SU1-IDPTYP                            
100807                                                                          
100907     MOVE W41403-OBKR-IDDISTR    TO SU1-IDDISTR                           
101007     MOVE W41403-OBKR-IDKUNDNR   TO SU1-IDKUNDNR                          
101107     MOVE WS-RED-IDSUPPL-SU      TO SU1-IDSUPPL                           
101207                                                                          
101307     MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU1-IDORDNR-002                   
101407                                                                          
101507     MOVE W41403-OBKR-IDARTNR    TO SU1-IDARTNR                           
101607     MOVE W41403-OBKR-REKSIFFR   TO SU1-REKSIFFR                          
101707     MOVE W41403-OBKR-KVBEART    TO SU1-KVBEART-002                       
101807     MOVE W41403-OBKR-KDORDKL    TO SU1-KDORDER                           
101907                                                                          
102007     IF  (W41403-OBKR-KDTPOTYP = +6                                       
102107      AND W41403-OBKR-KDORDBEK = 70)                                      
102207       MOVE ZERO                 TO SU1-KDTPOTYP                          
102307     ELSE                                                                 
102407       MOVE W41403-OBKR-KDTPOTYP TO SU1-KDTPOTYP                          
102507     END-IF                                                               
102607                                                                          
102707     MOVE W41403-OBKR-KDFAKTYP   TO SU1-KDFAKTYP                          
102807                                                                          
102907     IF  W41403-OBKR-KDPRTYP = 'P'                                        
103007       MOVE 'M'                  TO SU1-KDMANPR                           
103107     ELSE                                                                 
103207       MOVE W41403-OBKR-FLPRTILL TO SU1-KDMANPR                           
103307     END-IF                                                               
103407                                                                          
103507     MOVE WS-RED-KDVRTPO         TO SU1-KDVRTPO                           
103607                                                                          
103707     MOVE W41403-OBKR-KDVRINFO   TO SU1-KDVRINFO                          
103807                                                                          
103907     MOVE W41403-OBKR-TIREGDAT   TO SU1-TIAAMMDD-REG                      
104007     MOVE W41403-OBKR-TIKLOCK    TO SU1-TIKLOCK-REG                       
104107                                                                          
104207     PERFORM S13-SKRIV-W41406-SU1                                         
104307     .                                                                    
104407     EJECT                                                                
104507 DAB-NOAC-015-BYPASS SECTION.                                             
104607                                                                          
104707     MOVE SPACE                  TO W41408-AREA                           
104807                                                                          
104907     MOVE '015'                  TO W41408-BYPASS-IDPTYP                  
105007                                                                          
105107     MOVE W41403-OBKR-IDDISTR    TO W41408-BYPASS-IDDISTR                 
105207     MOVE W41403-OBKR-IDKUNDNR   TO W41408-BYPASS-IDKUNDNR                
105307                                                                          
105407     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W41408-BYPASS-IDORDNR             
105507                                                                          
105607     MOVE W41403-OBKR-KDORDKL    TO W41408-BYPASS-KDORDKL                 
105707     MOVE W41403-OBKR-IDARTNR    TO W41408-BYPASS-IDARTNR                 
105807     MOVE W41403-OBKR-REKSIFFR   TO W41408-BYPASS-REKSIFFR                
105907     MOVE W41403-OBKR-BERADREF   TO W41408-BYPASS-BERADREF                
106007     MOVE W41403-OBKR-BEVOLREF   TO W41408-BYPASS-BEVOLREF                
106107     MOVE W41403-OBKR-KVBEART    TO W41408-BYPASS-KVBEART                 
106207     MOVE W41403-OBKR-KDFAKTYP   TO W41408-BYPASS-KDFAKTYP                
106307     MOVE W41403-OBKR-KDDSP      TO W41408-BYPASS-KDDSP                   
106407     MOVE 'N'                    TO W41408-BYPASS-FLABON                  
106507                                                                          
106607     IF  (W41403-OBKR-KDTPOTYP = +6                                       
106707      AND W41403-OBKR-KDORDBEK = 70)                                      
106807       MOVE ZERO                 TO W41408-BYPASS-KDTPOTYP                
106907     ELSE                                                                 
107007       MOVE W41403-OBKR-KDTPOTYP TO W41408-BYPASS-KDTPOTYP                
107107     END-IF                                                               
107207                                                                          
107307     PERFORM S18-SKRIV-W41408                                             
107407     .                                                                    
107507     EJECT                                                                
107607 DAC-NOAC-020-ONORD SECTION.                                              
107707                                                                          
107807     MOVE SPACE                  TO W41409-AREA                           
107907                                                                          
108007     MOVE '020'                  TO W41409-ONORD-IDPTYP                   
108107                                                                          
108207     MOVE W41403-OBKR-IDDISTR    TO W41409-ONORD-IDDISTR                  
108307     MOVE W41403-OBKR-IDKUNDNR   TO W41409-ONORD-IDKUNDNR                 
108407                                                                          
108507     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W41409-ONORD-IDORDNR              
108607                                                                          
108707     MOVE W41403-OBKR-IDDC       TO W41409-ONORD-IDDC                     
108807     MOVE W41403-OBKR-KDORDKL    TO W41409-ONORD-KDORDKL                  
108907     MOVE W41403-OBKR-BEVARREF   TO W41409-ONORD-BEVARREF                 
109007     MOVE W41403-OBKR-BEVOLREF   TO W41409-ONORD-BEVOLREF                 
109107     MOVE W41403-OBKR-BERADREF   TO W41409-ONORD-BERADREF                 
109207     MOVE W41403-OBKR-IDARTNR    TO W41409-ONORD-IDARTNR                  
109307     MOVE W41403-OBKR-REKSIFFR   TO W41409-ONORD-REKSIFFR                 
109407     MOVE W41403-OBKR-KVBEART    TO W41409-ONORD-KVBEART                  
109507     MOVE W41403-OBKR-TIORDREG   TO W41409-ONORD-TIORDREG                 
109607     MOVE ZERO                   TO W41409-ONORD-IDRONR                   
109707     MOVE ZERO                   TO W41409-ONORD-TIRODAT                  
109807     MOVE W41403-OBKR-PRARTNTO   TO W41409-ONORD-PRARTNTO                 
109907     MOVE W41403-OBKR-PRARTBTO-EXP TO W41409-ONORD-PRARTBTO-EXP           
110007     MOVE W41403-OBKR-KDPRODSL   TO W41409-ONORD-KDPRODSL                 
110107     MOVE W41403-OBKR-IDFKNGRP   TO W41409-ONORD-IDFKNGRP                 
110207     MOVE W41403-OBKR-FLINVEST   TO W41409-ONORD-FLINVEST                 
110307     MOVE W41403-OBKR-KDDSP      TO W41409-ONORD-KDDSP                    
110407                                                                          
110507     IF  W41403-OBKR-KDPRTYP = 'P'                                        
110607       MOVE 'M'                  TO W41409-ONORD-KDMANPR                  
110707     ELSE                                                                 
110807       MOVE W41403-OBKR-FLPRTILL TO W41409-ONORD-KDMANPR                  
110907     END-IF                                                               
111007                                                                          
111107     MOVE 'N'                    TO W41409-ONORD-FLABON                   
111207                                                                          
111307     IF  (W41403-OBKR-KDTPOTYP = +6                                       
111407      AND W41403-OBKR-KDORDBEK = 70)                                      
111507       MOVE ZERO                 TO W41409-ONORD-KDTPOTYP                 
111607     ELSE                                                                 
111707       MOVE W41403-OBKR-KDTPOTYP TO W41409-ONORD-KDTPOTYP                 
111807     END-IF                                                               
111907                                                                          
112007     PERFORM S19-SKRIV-W41409                                             
112107     .                                                                    
112207     EJECT                                                                
112307 DB-SRS-VOR SECTION.                                                      
112407                                                                          
112507     MOVE SPACE                  TO W4140I-AREA                           
112607                                                                          
112707     IF W41403-OBKR-IDARTNR-TILLK > ZERO                                  
112807       MOVE W41403-OBKR-IDARTNR-TILLK TO W4140I-IDARTNR                   
112907     ELSE                                                                 
113007       MOVE W41403-OBKR-IDARTNR  TO W4140I-IDARTNR                        
113107     END-IF                                                               
113207     MOVE W41403-OBKR-IDDC       TO W4140I-IDDC                           
113307     MOVE W41403-OBKR-IDDISTR    TO W4140I-IDDISTR                        
113407     MOVE W41403-OBKR-IDKUNDNR   TO W4140I-IDKUNDNR                       
113507     MOVE W41403-OBKR-KDORDBEK   TO W4140I-KDORDBEK                       
113607     MOVE W41403-OBKR-KDORDKL    TO W4140I-KDORDKL                        
113707     MOVE 1                      TO W4140I-KDORDING                       
113807     MOVE W41403-OBKR-KDPRODSL   TO W4140I-KDPRODSL                       
113907     MOVE NEJ                    TO W4140I-FLDIRLEV                       
114007     MOVE NEJ                    TO W4140I-FLFORBI                        
114107     MOVE NEJ                    TO W4140I-FLORDSPE                       
114207     MOVE W41403-OBKR-FLOVRLEV   TO W4140I-FLOVRLEV                       
114307                                                                          
114407     PERFORM S38-SKRIV-W4140I                                             
114507     .                                                                    
114607     EJECT                                                                
114707 DE-NOAC-001-BIP SECTION.                                                 
114807                                                                          
114907     MOVE SPACE                  TO W41407-AREA                           
115007                                                                          
115107     MOVE '001'                  TO W41407-BIP-IDPTYP                     
115207                                                                          
115307     MOVE W41403-OBKR-IDDC       TO W41407-BIP-IDDC                       
115407     MOVE W41403-OBKR-IDDISTR    TO W41407-BIP-IDDISTR                    
115507     MOVE W41403-OBKR-IDKUNDNR   TO W41407-BIP-IDKUNDNR                   
115607     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W41407-BIP-IDORDNR                
115707     MOVE W41403-OBKR-KDORDKL    TO W41407-BIP-KDORDKL                    
115807     MOVE W41403-OBKR-IDARTNR    TO W41407-BIP-IDARTNR                    
115907     MOVE W41403-OBKR-REKSIFFR   TO W41407-BIP-REKSIFFR                   
116007     MOVE W41403-OBKR-BERADREF   TO W41407-BIP-BERADREF                   
116107                                                                          
116207     MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO W41407-BIP-IDRONR              
116307                                                                          
116407     MOVE ZERO                   TO W41407-BIP-TIRODAT                    
116507     MOVE W41403-OBKR-BEVOLREF   TO W41407-BIP-BEVOLREF                   
116607                                                                          
116707     MOVE WS-RED-KVBEART-Q-OBK92 TO W41407-BIP-KVLEVART                   
116807                                                                          
116907     MOVE W41403-OBKR-KDFAKTYP   TO W41407-BIP-KDFAKTYP                   
117007     MOVE WS-KDRESTR             TO W41407-BIP-KDRESTR                    
117107     MOVE W41403-OBKR-TIREGDAT   TO W41407-BIP-TIAAMMDD                   
117207     MOVE W41403-OBKR-TIKLOCK    TO W41407-BIP-TIKLOCK                    
117307                                                                          
117407     PERFORM S17-SKRIV-W41407                                             
117507     .                                                                    
117607     EJECT                                                                
117707 DF-VR-SUA SECTION.                                                       
117807                                                                          
117907     MOVE SPACE                  TO SUA-AREA                              
118007                                                                          
118107     MOVE 'SUA'                  TO SUA-IDPTYP                            
118207                                                                          
118307     MOVE W41403-OBKR-IDDISTR    TO SUA-IDDISTR                           
118407     MOVE W41403-OBKR-IDKUNDNR   TO SUA-IDKUNDNR                          
118507     MOVE WS-RED-IDSUPPL-SU      TO SUA-IDSUPPL                           
118607                                                                          
118707     MOVE W41403-OBKR-IDKUNDRF (1:7) TO SUA-IDORDNR-002                   
118807                                                                          
118907     MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO SUA-IDRONR-002                 
119007                                                                          
119107     MOVE W41403-OBKR-IDARTNR    TO SUA-IDARTNR                           
119207     MOVE W41403-OBKR-REKSIFFR   TO SUA-REKSIFFR                          
119307                                                                          
119407     MOVE WS-RED-KVBEART-Q-OBK92 TO SUA-KVLEVART                          
119507                                                                          
119607     MOVE WS-KDRESTR             TO SUA-KDRESTR                           
119707                                                                          
119807     MOVE W41403-OBKR-TIREGDAT   TO SUA-TIAAMMDD                          
119907     MOVE W41403-OBKR-TIKLOCK    TO SUA-TIKLOCK                           
120007                                                                          
120107     PERFORM S11-SKRIV-W41406-SUA                                         
120207     .                                                                    
120307     EJECT                                                                
120407 DG-NOAC-006-OBKVAN SECTION.                                              
120507                                                                          
120607     MOVE SPACE                  TO W4140A-OBKVAN-W461006                 
120707                                                                          
120807     MOVE '006'                  TO W4140A-OBKVAN-IDPTYP                  
120907                                                                          
121007     MOVE W41403-OBKR-IDDISTR    TO W4140A-OBKVAN-IDDISTR                 
121107     MOVE W41403-OBKR-IDKUNDNR   TO W4140A-OBKVAN-IDKUNDNR                
121207     MOVE W41403-OBKR-KDFRAKT    TO W4140A-OBKVAN-KDFRAKT                 
121307     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W4140A-OBKVAN-IDORDNR             
121407                                                                          
121507     IF  W41403-OBKR-IDKUNDRF-RO = K-IDKUNDRF-NOLL-LNG7                   
121607       MOVE ZERO                 TO W4140A-OBKVAN-KDLIDEL                 
121707     ELSE                                                                 
121807       MOVE +1                   TO W4140A-OBKVAN-KDLIDEL                 
121907     END-IF                                                               
122007                                                                          
122107     MOVE W41403-OBKR-IDDC       TO W4140A-OBKVAN-IDDC                    
122207     IF W41403-OBKR-IDARTNR-TILLK > ZERO                                  
122307        MOVE W41403-OBKR-IDARTNR-TILLK  TO W4140A-OBKVAN-IDARTNR          
122407        MOVE W41403-OBKR-REKSIFFR-TILLK TO W4140A-OBKVAN-REKSIFFR         
122507     ELSE                                                                 
122607        MOVE W41403-OBKR-IDARTNR    TO W4140A-OBKVAN-IDARTNR              
122707        MOVE W41403-OBKR-REKSIFFR   TO W4140A-OBKVAN-REKSIFFR             
122807     END-IF                                                               
122907     MOVE SPACE                  TO W4140A-OBKVAN-BEART                   
123007     MOVE W41403-OBKR-BERADREF   TO W4140A-OBKVAN-BERADREF                
123107                                                                          
123207     MOVE W41403-OBKR-IDKUNDRF-RO (1:7)                                   
123307                                 TO W4140A-OBKVAN-IDRONR                  
123407     MOVE ZERO                   TO W4140A-OBKVAN-TIRODAT                 
123507     MOVE W41403-OBKR-BEVOLREF   TO W4140A-OBKVAN-BEVOLREF                
123607     MOVE W41403-OBKR-KDORDBEK   TO W4140A-OBKVAN-KDRESTR                 
123707                                                                          
123807     MOVE WS-RED-KVBEART-OBK92   TO W4140A-OBKVAN-KVBEART                 
123907     MOVE WS-RED-KVBEART-Q-OBK92 TO W4140A-OBKVAN-KVBEART-Q               
124007                                                                          
124107     MOVE W41403-OBKR-KVQPACK-1  TO W4140A-OBKVAN-KVQPACK-1               
124207     MOVE W41403-OBKR-KDKVBRYT   TO W4140A-OBKVAN-KDKVBRYT                
124307     MOVE W41403-OBKR-KDDSP      TO W4140A-OBKVAN-KDDSP                   
124407     MOVE W41403-OBKR-KDFAKTYP   TO W4140A-OBKVAN-KDFAKTYP                
124507     MOVE W41403-OBKR-TIREGDAT   TO W4140A-OBKVAN-TIAAMMDD                
124607     MOVE W41403-OBKR-TIKLOCK    TO W4140A-OBKVAN-TIKLOCK                 
124707                                                                          
124807     PERFORM S26-SKRIV-W4140A-006                                         
124907     .                                                                    
125007     EJECT                                                                
125107 DH-NOAC-008-OBSTOP-BEST SECTION.                                         
125207                                                                          
125307     MOVE SPACE                  TO W4140A-OBSTOP-W461008                 
125407                                                                          
125507     MOVE '008'                  TO W4140A-OBSTOP-IDPTYP                  
125607                                                                          
125707     MOVE W41403-OBKR-IDDISTR    TO W4140A-OBSTOP-IDDISTR                 
125807     MOVE W41403-OBKR-IDKUNDNR   TO W4140A-OBSTOP-IDKUNDNR                
125907     MOVE W41403-OBKR-KDFRAKT    TO W4140A-OBSTOP-KDFRAKT                 
126007                                                                          
126107     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W4140A-OBSTOP-IDORDNR             
126207                                                                          
126307     IF  W41403-OBKR-IDKUNDRF-RO = K-IDKUNDRF-NOLL-LNG7                   
126407       MOVE +0                   TO W4140A-OBSTOP-KDLIDEL                 
126507     ELSE                                                                 
126607       MOVE +1                   TO W4140A-OBSTOP-KDLIDEL                 
126707     END-IF                                                               
126807                                                                          
126907     MOVE W41403-OBKR-IDDC       TO W4140A-OBSTOP-IDDC                    
127007     IF W41403-OBKR-IDARTNR-TILLK > ZERO                                  
127107        MOVE W41403-OBKR-IDARTNR-TILLK  TO W4140A-OBSTOP-IDARTNR          
127207        MOVE W41403-OBKR-REKSIFFR-TILLK TO W4140A-OBSTOP-REKSIFFR         
127307     ELSE                                                                 
127407        MOVE W41403-OBKR-IDARTNR    TO W4140A-OBSTOP-IDARTNR              
127507        MOVE W41403-OBKR-REKSIFFR   TO W4140A-OBSTOP-REKSIFFR             
127607     END-IF                                                               
127707     MOVE SPACE                  TO W4140A-OBSTOP-BEART                   
127807     MOVE W41403-OBKR-BERADREF   TO W4140A-OBSTOP-BERADREF                
127907     MOVE W41403-OBKR-BEVOLREF   TO W4140A-OBSTOP-BEVOLREF                
128007                                                                          
128107     MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO W4140A-OBSTOP-IDRONR           
128207                                                                          
128307     MOVE ZERO                   TO W4140A-OBSTOP-TIRODAT                 
128407     MOVE W41403-OBKR-TIORDREG   TO W4140A-OBSTOP-TIORDREG                
128507     MOVE W41403-OBKR-KDDSP      TO W4140A-OBSTOP-KDDSP                   
128607     MOVE W41403-OBKR-KDKVBRYT   TO W4140A-OBSTOP-KDKVBRYT                
128707     MOVE W41403-OBKR-KDORDBEK TO W4140A-OBSTOP-KDRESTR                   
128807                                                                          
128907     MOVE WS-RED-KVBEART-OBK92   TO W4140A-OBSTOP-KVBEART                 
129007                                                                          
129107     MOVE W41403-OBKR-KDFAKTYP   TO W4140A-OBSTOP-KDFAKTYP                
129207     MOVE W41403-OBKR-TIREGDAT   TO W4140A-OBSTOP-TIAAMMDD                
129307     MOVE W41403-OBKR-TIKLOCK    TO W4140A-OBSTOP-TIKLOCK                 
129407                                                                          
129507     PERFORM S28-SKRIV-W4140A-008                                         
129607     .                                                                    
129707     EJECT                                                                
129807 DI-VR-SU2-UTG SECTION.                                                   
129907                                                                          
130007     MOVE SPACE                  TO SU2-AREA                              
130107                                                                          
130207     MOVE 'SU2'                  TO SU2-IDPTYP                            
130307                                                                          
130407     MOVE W41403-OBKR-IDDISTR    TO SU2-IDDISTR                           
130507     MOVE W41403-OBKR-IDKUNDNR   TO SU2-IDKUNDNR                          
130607     MOVE WS-RED-IDSUPPL-SU      TO SU2-IDSUPPL                           
130707                                                                          
130807     MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU2-IDORDNR-002                   
130907                                                                          
131007     IF  W41403-OBKR-IDARTNR-TILLK > ZERO                                 
131107       MOVE W41403-OBKR-IDARTNR-TILLK TO SU2-IDARTNR-ERS                  
131207       MOVE W41403-OBKR-REKSIFFR-TILLK TO SU2-REKSIFFR-ERS                
131307     ELSE                                                                 
131407       MOVE W41403-OBKR-IDARTNR  TO SU2-IDARTNR-ERS                       
131507       MOVE W41403-OBKR-REKSIFFR TO SU2-REKSIFFR-ERS                      
131607     END-IF                                                               
131707                                                                          
131807     MOVE ZERO                   TO SU2-IDARTNR-TILLK                     
131907     MOVE ZERO                   TO SU2-REKSIFFR-TILLK                    
132007                                                                          
132107     MOVE WS-RED-KVBEART-OBK92   TO SU2-KVBEART-002                       
132207                                                                          
132307     MOVE ZERO                   TO SU2-DIERS                             
132407                                    SU2-KVLEVART-002                      
132507     MOVE W41403-OBKR-KDORDBEK   TO SU2-KDRESTR                           
132607     MOVE ZERO                   TO SU2-KDRO                              
132707     MOVE W41403-OBKR-KDORDKL    TO SU2-KDORDER                           
132807     MOVE ZERO                   TO SU2-FLVRERS                           
132907                                                                          
133007     MOVE WS-RED-KDERS           TO SU2-KDERS                             
133107                                                                          
133207     MOVE W41403-OBKR-KDORDKL    TO SU2-KDORDKL                           
133307     MOVE W41403-OBKR-KDFAKTYP   TO SU2-KDFAKTYP                          
133407     MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU2-IDORDNR7-LEV                  
133507     MOVE W41403-OBKR-KDTPOTYP   TO SU2-KDTPOTYP                          
133607                                                                          
133707     MOVE WS-RED-KDVRTPO         TO SU2-KDVRTPO                           
133807                                                                          
133907     MOVE W41403-OBKR-KDVRINFO   TO SU2-KDVRINFO                          
134007                                                                          
134107     MOVE W41403-OBKR-TIREGDAT   TO SU2-TIAAMMDD                          
134207     MOVE W41403-OBKR-TIKLOCK    TO SU2-TIKLOCK                           
134307                                                                          
134407     PERFORM S14-SKRIV-W41406-SU2                                         
134507     .                                                                    
134607     EJECT                                                                
134707 DJ-VR-SU3 SECTION.                                                       
134807                                                                          
134907     MOVE SPACE                  TO SU3-AREA                              
135007                                                                          
135107     MOVE 'SU3'                  TO SU3-IDPTYP                            
135207                                                                          
135307     IF  W41403-OBKR-KDORDBEK = 43 OR 44                                  
135407       COMPUTE SU3-KVBEART-002 = W41403-OBKR-KVBEART-Q                    
135507                           - W41403-OBKR-KVBEART                          
135607       END-COMPUTE                                                        
135707       IF  W41403-OBKR-KDORDBEK = 43                                      
135807         MOVE 1                  TO SU3-KDKVFOR                           
135907       ELSE                                                               
136007         MOVE 2                  TO SU3-KDKVFOR                           
136107       END-IF                                                             
136207     ELSE                                                                 
136307       MOVE 2                    TO SU3-KDKVFOR                           
136407       IF  W41403-OBKR-KDORDBEK = 92                                      
136507         MOVE W41403-OBKR-KVPRERO  TO SU3-KVBEART-002                     
136607       ELSE                                                               
136707         MOVE W41403-OBKR-KVBEART  TO SU3-KVBEART-002                     
136807       END-IF                                                             
136907     END-IF                                                               
137007                                                                          
137107     MOVE W41403-OBKR-KDORDBEK TO SU3-KDRESTR                             
137207                                                                          
137307     MOVE W41403-OBKR-IDDISTR    TO SU3-IDDISTR                           
137407     MOVE W41403-OBKR-IDKUNDNR   TO SU3-IDKUNDNR                          
137507     MOVE WS-RED-IDSUPPL-SU      TO SU3-IDSUPPL                           
137607                                                                          
137707     IF W41403-OBKR-KDORDBEK = 61                                         
137807       IF  (W41403-OBKR-KDTPOTYP =  1 OR 2 OR 3 OR 4 OR 5)                
137907         MOVE ZERO                 TO SU3-IDORDNR-002                     
138007                                      SU3-IDORDNR7-LEV                    
138107       ELSE                                                               
138207         IF  W41403-OBKR-IDKUNDRF-RO NOT = K-IDKUNDRF-NOLL-LNG7           
138307           MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO SU3-IDORDNR-002          
138407         ELSE                                                             
138507           MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU3-IDORDNR-002             
138607         END-IF                                                           
138707         MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU3-IDORDNR7-LEV              
138807       END-IF                                                             
138907     ELSE                                                                 
139007       IF  W41403-OBKR-IDKUNDRF-RO NOT = K-IDKUNDRF-NOLL-LNG7             
139107         MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO SU3-IDORDNR-002            
139207       ELSE                                                               
139307         MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU3-IDORDNR-002               
139407       END-IF                                                             
139507       MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU3-IDORDNR7-LEV                
139607     END-IF                                                               
139707                                                                          
139807     IF W41403-OBKR-IDARTNR-TILLK > ZERO                                  
139907        MOVE W41403-OBKR-IDARTNR-TILLK  TO SU3-IDARTNR                    
140007        MOVE W41403-OBKR-REKSIFFR-TILLK TO SU3-REKSIFFR                   
140107     ELSE                                                                 
140207        MOVE W41403-OBKR-IDARTNR    TO SU3-IDARTNR                        
140307        MOVE W41403-OBKR-REKSIFFR   TO SU3-REKSIFFR                       
140407     END-IF                                                               
140507     MOVE W41403-OBKR-KDFAKTYP   TO SU3-KDFAKTYP                          
140607     MOVE ZERO                   TO SU3-KDRO                              
140707     MOVE W41403-OBKR-KDORDKL    TO SU3-KDORDER                           
140807     IF  W41403-OBKR-KDORDBEK = 43 OR 44                                  
140907         MOVE W41403-OBKR-KVQPACK-1  TO SU3-KVQPACK-1                     
141007     ELSE                                                                 
141107         MOVE ZERO                   TO SU3-KVQPACK-1                     
141207     END-IF                                                               
141307     MOVE NEJ                    TO SU3-TID-ERS                           
141407     MOVE W41403-OBKR-KDORDKL    TO SU3-KDORDKL                           
141507     MOVE ZERO                   TO SU3-FIKTIV-KVANT                      
141607                                                                          
141707     MOVE WS-RED-KDVRTPO         TO SU3-KDVRTPO                           
141807                                                                          
141907     MOVE W41403-OBKR-KDVRINFO   TO SU3-KDVRINFO                          
142007                                                                          
142107     MOVE W41403-OBKR-TIREGDAT   TO SU3-TIAAMMDD                          
142207     MOVE W41403-OBKR-TIKLOCK    TO SU3-TIKLOCK                           
142307                                                                          
142407     PERFORM S15-SKRIV-W41406-SU3                                         
142507     .                                                                    
142607     EJECT                                                                
142707 DK-NOAC-009-SERV SECTION.                                                
142807                                                                          
142907     MOVE SPACE                  TO W4140G-AREA                           
143007                                                                          
143107     MOVE '009'                  TO W4140G-SERV-IDPTYP                    
143207                                                                          
143307     MOVE W41403-OBKR-IDDC       TO W4140G-SERV-IDDC                      
143407     MOVE W41403-OBKR-IDDISTR    TO W4140G-SERV-IDDISTR                   
143507     MOVE W41403-OBKR-IDKUNDNR   TO W4140G-SERV-IDKUNDNR                  
143607     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W4140G-SERV-IDORDNR               
143707     MOVE W41403-OBKR-KDORDKL    TO W4140G-SERV-KDORDKL                   
143807     IF W41403-OBKR-IDARTNR-TILLK > ZERO                                  
143907        MOVE W41403-OBKR-IDARTNR-TILLK  TO W4140G-SERV-IDARTNR            
144007        MOVE W41403-OBKR-REKSIFFR-TILLK TO W4140G-SERV-REKSIFFR           
144107     ELSE                                                                 
144207        MOVE W41403-OBKR-IDARTNR    TO W4140G-SERV-IDARTNR                
144307        MOVE W41403-OBKR-REKSIFFR   TO W4140G-SERV-REKSIFFR               
144407     END-IF                                                               
144507     MOVE W41403-OBKR-TIORDREG   TO W4140G-SERV-TIORDREG                  
144607     MOVE W41403-OBKR-KDPRODSL   TO W4140G-SERV-KDPRODSL                  
144707                                                                          
144807     MOVE WS-RED-KVBEART-Q-OBK92 TO W4140G-SERV-KVBEART                   
144907                                                                          
145007     MOVE W41403-OBKR-KDFAKTYP   TO W4140G-SERV-KDFAKTYP                  
145107                                                                          
145207     PERFORM S36-SKRIV-W4140G                                             
145307     .                                                                    
145407     EJECT                                                                
145507 DL-NOAC-007-OBLAG SECTION.                                               
145607                                                                          
145707     MOVE SPACE                  TO W4140A-OBLAG-W461007                  
145807                                                                          
145907     MOVE '007'                  TO W4140A-OBLAG-IDPTYP                   
146007                                                                          
146107     MOVE W41403-OBKR-IDDISTR    TO W4140A-OBLAG-IDDISTR                  
146207     MOVE W41403-OBKR-IDKUNDNR   TO W4140A-OBLAG-IDKUNDNR                 
146307     MOVE W41403-OBKR-KDFRAKT    TO W4140A-OBLAG-KDFRAKT                  
146407     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W4140A-OBLAG-IDORDNR              
146507                                                                          
146607     IF  W41403-OBKR-FLTILLK = NEJ                                        
146707       IF  W41403-OBKR-IDKUNDRF-RO = K-IDKUNDRF-NOLL-LNG7                 
146807         MOVE +0                 TO W4140A-OBLAG-KDLIDEL                  
146907       ELSE                                                               
147007         MOVE +1                 TO W4140A-OBLAG-KDLIDEL                  
147107       END-IF                                                             
147207     ELSE                                                                 
147307       MOVE +2                   TO W4140A-OBLAG-KDLIDEL                  
147407     END-IF                                                               
147507                                                                          
147607     MOVE W41403-OBKR-IDDC       TO W4140A-OBLAG-IDDC                     
147707     IF W41403-OBKR-IDARTNR-TILLK > ZERO                                  
147807        MOVE W41403-OBKR-IDARTNR-TILLK  TO W4140A-OBLAG-IDARTNR           
147907        MOVE W41403-OBKR-REKSIFFR-TILLK TO W4140A-OBLAG-REKSIFFR          
148007     ELSE                                                                 
148107        MOVE W41403-OBKR-IDARTNR    TO W4140A-OBLAG-IDARTNR               
148207        MOVE W41403-OBKR-REKSIFFR   TO W4140A-OBLAG-REKSIFFR              
148307     END-IF                                                               
148407     MOVE SPACE                  TO W4140A-OBLAG-BEART                    
148507     MOVE W41403-OBKR-BERADREF   TO W4140A-OBLAG-BERADREF                 
148607     MOVE W41403-OBKR-BEVOLREF   TO W4140A-OBLAG-BEVOLREF                 
148707                                                                          
148807     IF  W41403-OBKR-IDKUNDRF-RO NOT = K-IDKUNDRF-NOLL-LNG7               
148907       MOVE W41403-OBKR-IDKUNDRF-RO (1:7)                                 
149007                                 TO W4140A-OBLAG-IDRONR                   
149107     ELSE                                                                 
149207       MOVE W41403-OBKR-IDKUNDRF(1:7)                                     
149307                                 TO W4140A-OBLAG-IDRONR                   
149407     END-IF                                                               
149507                                                                          
149607     MOVE ZERO                   TO W4140A-OBLAG-TIRODAT                  
149707     MOVE W41403-OBKR-TIORDREG   TO W4140A-OBLAG-TIORDREG                 
149807     MOVE W41403-OBKR-KDORDBEK   TO W4140A-OBLAG-KDRESTR                  
149907     MOVE WS-RED-KVBEART-OBK92   TO W4140A-OBLAG-KVBEART                  
150007                                                                          
150107     IF  W41403-OBKR-KDORDBEK = 92                                        
150207       MOVE WS-RED-KVBEART-Q-OBK92 TO W4140A-OBLAG-KVAVBART               
150307     ELSE                                                                 
150407       MOVE ZERO                 TO W4140A-OBLAG-KVAVBART                 
150507     END-IF                                                               
150607                                                                          
150707     MOVE W41403-OBKR-KDDSP      TO W4140A-OBLAG-KDDSP                    
150807     MOVE ZERO                   TO W4140A-OBLAG-KVRO                     
150907     MOVE W41403-OBKR-KDFAKTYP   TO W4140A-OBLAG-KDFAKTYP                 
151007     MOVE W41403-OBKR-TIDISPIN   TO W4140A-OBLAG-TIDISPIN                 
151107     MOVE W41403-OBKR-KDKVBRYT   TO W4140A-OBLAG-KDKVBRYT                 
151207     MOVE W41403-OBKR-TIREGDAT   TO W4140A-OBLAG-TIAAMMDD                 
151307     MOVE W41403-OBKR-TIKLOCK    TO W4140A-OBLAG-TIKLOCK                  
151407                                                                          
151507     PERFORM S27-SKRIV-W4140A-007                                         
151607     .                                                                    
151707     EJECT                                                                
151807 DN-VR-SU2-TILLK SECTION.                                                 
151907                                                                          
152007     MOVE SPACE                  TO SU2-AREA                              
152107                                                                          
152207     MOVE 'SU2'                  TO SU2-IDPTYP                            
152307                                                                          
152407     MOVE W41403-OBKR-IDDISTR    TO SU2-IDDISTR                           
152507     MOVE W41403-OBKR-IDKUNDNR   TO SU2-IDKUNDNR                          
152607     MOVE WS-RED-IDSUPPL-SU      TO SU2-IDSUPPL                           
152707                                                                          
152807     IF  (W41403-OBKR-KDTPOTYP =  1 OR 2 OR 3 OR 4 OR 5)                  
152907         MOVE ZERO               TO SU2-IDORDNR-002                       
153007                                    SU2-IDORDNR7-LEV                      
153107                                    SU2-KVBEART-002                       
153207                                    SU2-KVLEVART-002                      
153307     ELSE                                                                 
153407       MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU2-IDORDNR-002                 
153507       MOVE W41403-OBKR-IDKUNDRF (1:7) TO SU2-IDORDNR7-LEV                
153607                                                                          
153707       COMPUTE SU2-KVBEART-002                                            
153807                        = W41403-OBKR-KVBEART-TILLK                       
153907                        / W41403-OBKR-DIERS-KVOT                          
154007       END-COMPUTE                                                        
154107       COMPUTE SU2-KVLEVART-002                                           
154207                        = SU2-KVBEART-002 * W41403-OBKR-DIERS-KVOT        
154307     END-IF                                                               
154407                                                                          
154507     MOVE W41403-OBKR-IDARTNR    TO SU2-IDARTNR-ERS                       
154607     MOVE W41403-OBKR-REKSIFFR   TO SU2-REKSIFFR-ERS                      
154707     MOVE W41403-OBKR-IDARTNR-TILLK TO SU2-IDARTNR-TILLK                  
154807     MOVE W41403-OBKR-REKSIFFR-TILLK TO SU2-REKSIFFR-TILLK                
154907     MOVE W41403-OBKR-DIERS-KVOT TO SU2-DIERS                             
155007     MOVE W41403-OBKR-KDORDBEK   TO SU2-KDRESTR                           
155107     MOVE ZERO                   TO SU2-KDRO                              
155207     MOVE W41403-OBKR-KDORDKL    TO SU2-KDORDER                           
155307     MOVE 1                      TO SU2-FLVRERS                           
155407     MOVE WS-RED-KDERS           TO SU2-KDERS                             
155507     MOVE W41403-OBKR-KDORDKL    TO SU2-KDORDKL                           
155607     MOVE W41403-OBKR-KDFAKTYP   TO SU2-KDFAKTYP                          
155707     MOVE W41403-OBKR-KDTPOTYP   TO SU2-KDTPOTYP                          
155807                                                                          
155907     MOVE WS-RED-KDVRTPO         TO SU2-KDVRTPO                           
156007                                                                          
156107     MOVE W41403-OBKR-KDVRINFO   TO SU2-KDVRINFO                          
156207                                                                          
156307     MOVE W41403-OBKR-TIREGDAT   TO SU2-TIAAMMDD                          
156407     MOVE W41403-OBKR-TIKLOCK    TO SU2-TIKLOCK                           
156507                                                                          
156607     PERFORM S14-SKRIV-W41406-SU2                                         
156707     .                                                                    
156807     EJECT                                                                
156907 DO-NOAC-003-OBEN SECTION.                                                
157007                                                                          
157107     MOVE SPACE                  TO W4140A-OBEN-W461003                   
157207                                                                          
157307     MOVE '003'                  TO W4140A-OBEN-IDPTYP                    
157407                                                                          
157507     MOVE W41403-OBKR-IDDISTR    TO W4140A-OBEN-IDDISTR                   
157607     MOVE W41403-OBKR-IDKUNDNR   TO W4140A-OBEN-IDKUNDNR                  
157707     MOVE W41403-OBKR-KDFRAKT    TO W4140A-OBEN-KDFRAKT                   
157807     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W4140A-OBEN-IDORDNR               
157907                                                                          
158007     MOVE +2                     TO W4140A-OBEN-KDLIDEL                   
158107                                                                          
158207     MOVE W41403-OBKR-IDDC       TO W4140A-OBEN-IDDC                      
158307     MOVE W41403-OBKR-IDARTNR    TO W4140A-OBEN-IDARTNR                   
158407     MOVE W41403-OBKR-REKSIFFR   TO W4140A-OBEN-REKSIFFR                  
158507     MOVE SPACE                  TO W4140A-OBEN-BEART                     
158607     MOVE W41403-OBKR-IDLOPNR    TO W4140A-OBEN-IDLOPNRE                  
158707     MOVE W41403-OBKR-IDSEKVNR   TO W4140A-OBEN-IDKORTNR                  
158807     MOVE W41403-OBKR-BERADREF   TO W4140A-OBEN-BERADREF                  
158907     MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO W4140A-OBEN-IDRONR             
159007     MOVE ZERO                   TO W4140A-OBEN-TIRODAT                   
159107     MOVE W41403-OBKR-BEVOLREF   TO W4140A-OBEN-BEVOLREF                  
159207     MOVE W41403-OBKR-KDORDBEK   TO W4140A-OBEN-KDRESTR                   
159307     MOVE WS-RED-KDERS           TO W4140A-OBEN-KDERS                     
159407                                                                          
159507     COMPUTE W4140A-OBEN-KVBEART = W41403-OBKR-KVBEART-TILLK              
159607                                 / W41403-OBKR-DIERS-KVOT                 
159707     END-COMPUTE                                                          
159807                                                                          
159907     MOVE W41403-OBKR-IDARTNR-TILLK TO W4140A-OBEN-IDARTNR-TILLK          
160007     MOVE W41403-OBKR-REKSIFFR-TILLK TO W4140A-OBEN-REKSIFFR-TILLK        
160107     MOVE W41403-OBKR-KVBEART-TILLK TO W4140A-OBEN-KVBEART-TILLK          
160207     MOVE W41403-OBKR-DIERS-KVOT TO W4140A-OBEN-DIERS-KVOT                
160307                                                                          
160407     IF  W41403-OBKR-KDERS <  10                                          
160507     OR  W41403-OBKR-KDERS =  17 OR 19 OR 27 OR 28 OR 29 OR 52            
160607       MOVE ZERO                 TO W4140A-OBEN-KDERSUP                   
160707     ELSE                                                                 
160807       MOVE +1                   TO W4140A-OBEN-KDERSUP                   
160907     END-IF                                                               
161007                                                                          
161107     MOVE W41403-OBKR-KDKVBRYT   TO W4140A-OBEN-KDKVBRYT                  
161207     MOVE W41403-OBKR-KDDSP      TO W4140A-OBEN-KDDSP                     
161307     MOVE W41403-OBKR-KDFAKTYP   TO W4140A-OBEN-KDFAKTYP                  
161407     MOVE ZERO                   TO W4140A-OBEN-KDRO                      
161507                                                                          
161607     PERFORM S23-SKRIV-W4140A-003                                         
161707     .                                                                    
161807     EJECT                                                                
161907 DP-NOAC-004-OBEEN SECTION.                                               
162007                                                                          
162107     MOVE SPACE                  TO W4140A-OBEEN-W461004                  
162207                                                                          
162307     MOVE '004'                  TO W4140A-OBEEN-IDPTYP                   
162407                                                                          
162507     MOVE W41403-OBKR-IDDISTR    TO W4140A-OBEEN-IDDISTR                  
162607     MOVE W41403-OBKR-IDKUNDNR   TO W4140A-OBEEN-IDKUNDNR                 
162707     MOVE W41403-OBKR-KDFRAKT    TO W4140A-OBEEN-KDFRAKT                  
162807     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W4140A-OBEEN-IDORDNR              
162907                                                                          
163007     MOVE +2                     TO W4140A-OBEEN-KDLIDEL                  
163107                                                                          
163207     MOVE W41403-OBKR-IDDC       TO W4140A-OBEEN-IDDC                     
163307     MOVE W41403-OBKR-IDARTNR    TO W4140A-OBEEN-IDARTNR                  
163407     MOVE W41403-OBKR-REKSIFFR   TO W4140A-OBEEN-REKSIFFR                 
163507     MOVE SPACE                  TO W4140A-OBEEN-BEART                    
163607     MOVE W41403-OBKR-IDLOPNR    TO W4140A-OBEEN-IDLOPNRE                 
163707     MOVE W41403-OBKR-IDSEKVNR   TO W4140A-OBEEN-IDKORTNR                 
163807     MOVE W41403-OBKR-BERADREF   TO W4140A-OBEEN-BERADREF                 
163907     MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO W4140A-OBEEN-IDRONR            
164007     MOVE ZERO                   TO W4140A-OBEEN-TIRODAT                  
164107     MOVE W41403-OBKR-BEVOLREF   TO W4140A-OBEEN-BEVOLREF                 
164207     MOVE W41403-OBKR-KDORDBEK   TO W4140A-OBEEN-KDRESTR                  
164307     MOVE WS-RED-KDERS           TO W4140A-OBEEN-KDERS                    
164407                                                                          
164507     COMPUTE W4140A-OBEEN-KVBEART = W41403-OBKR-KVBEART-TILLK             
164607                                  / W41403-OBKR-DIERS-KVOT                
164707     END-COMPUTE                                                          
164807                                                                          
164907     MOVE W41403-OBKR-IDARTNR-TILLK TO W4140A-OBEEN-IDARTNR-TILLK         
165007     MOVE W41403-OBKR-REKSIFFR-TILLK                                      
165107                                 TO W4140A-OBEEN-REKSIFFR-TILLK           
165207     MOVE W41403-OBKR-KVBEART-TILLK TO W4140A-OBEEN-KVBEART-TILLK         
165307     MOVE W41403-OBKR-DIERS-KVOT TO W4140A-OBEEN-DIERS-KVOT               
165407                                                                          
165507     IF  W41403-OBKR-KDERS <  10                                          
165607     OR  W41403-OBKR-KDERS =  17 OR 19 OR 27 OR 28 OR 29 OR 52            
165707       MOVE ZERO                 TO W4140A-OBEEN-KDERSUP                  
165807     ELSE                                                                 
165907       MOVE +1                   TO W4140A-OBEEN-KDERSUP                  
166007     END-IF                                                               
166107                                                                          
166207     MOVE W41403-OBKR-KDKVBRYT   TO W4140A-OBEEN-KDKVBRYT                 
166307     MOVE W41403-OBKR-KDDSP      TO W4140A-OBEEN-KDDSP                    
166407     MOVE W41403-OBKR-KDFAKTYP   TO W4140A-OBEEN-KDFAKTYP                 
166507     MOVE ZERO                   TO W4140A-OBEEN-KDRO                     
166607                                                                          
166707     PERFORM S24-SKRIV-W4140A-004                                         
166807     .                                                                    
166907     EJECT                                                                
167007 DQ-NOAC-008-OBSTOP-TILLK SECTION.                                        
167107                                                                          
167207     MOVE SPACE                  TO W4140A-OBSTOP-W461008                 
167307                                                                          
167407     MOVE '008'                  TO W4140A-OBSTOP-IDPTYP                  
167507                                                                          
167607     MOVE W41403-OBKR-IDDISTR    TO W4140A-OBSTOP-IDDISTR                 
167707     MOVE W41403-OBKR-IDKUNDNR   TO W4140A-OBSTOP-IDKUNDNR                
167807     MOVE W41403-OBKR-KDFRAKT    TO W4140A-OBSTOP-KDFRAKT                 
167907     MOVE W41403-OBKR-IDKUNDRF (1:7)  TO W4140A-OBSTOP-IDORDNR            
168007     MOVE ZERO                   TO W4140A-OBSTOP-KDLIDEL                 
168107     MOVE W41403-OBKR-IDDC       TO W4140A-OBSTOP-IDDC                    
168207     MOVE W41403-OBKR-IDARTNR-TILLK TO W4140A-OBSTOP-IDARTNR              
168307     MOVE W41403-OBKR-REKSIFFR-TILLK TO W4140A-OBSTOP-REKSIFFR            
168407     MOVE SPACE                  TO W4140A-OBSTOP-BEART                   
168507     MOVE W41403-OBKR-BERADREF   TO W4140A-OBSTOP-BERADREF                
168607     MOVE W41403-OBKR-BEVOLREF   TO W4140A-OBSTOP-BEVOLREF                
168707     MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO W4140A-OBSTOP-IDRONR           
168807     MOVE ZERO                   TO W4140A-OBSTOP-TIRODAT                 
168907     MOVE W41403-OBKR-TIREGDAT-OBKR TO W4140A-OBSTOP-TIORDREG             
169007     MOVE W41403-OBKR-KDDSP      TO W4140A-OBSTOP-KDDSP                   
169107     MOVE W41403-OBKR-KDKVBRYT   TO W4140A-OBSTOP-KDKVBRYT                
169207     MOVE W41403-OBKR-KDORDBEK   TO W4140A-OBSTOP-KDRESTR                 
169307                                                                          
169407     COMPUTE W4140A-OBSTOP-KVBEART = W41403-OBKR-KVBEART-TILLK            
169507                                   / W41403-OBKR-DIERS-KVOT               
169607     END-COMPUTE                                                          
169707                                                                          
169807     MOVE W41403-OBKR-KDFAKTYP   TO W4140A-OBSTOP-KDFAKTYP                
169907     MOVE W41403-OBKR-TIREGDAT   TO W4140A-OBSTOP-TIAAMMDD                
170007     MOVE W41403-OBKR-TIKLOCK    TO W4140A-OBSTOP-TIKLOCK                 
170107                                                                          
170207     PERFORM S28-SKRIV-W4140A-008                                         
170307     .                                                                    
170407     EJECT                                                                
170507 DR-NOAC-005-OBTEXT SECTION.                                              
170607                                                                          
170707     MOVE SPACE                  TO W4140A-OBTEXT-W461005                 
170807                                                                          
170907     MOVE '005'                  TO W4140A-OBTEXT-IDPTYP                  
171007                                                                          
171107     MOVE W41403-OBKR-IDDISTR    TO W4140A-OBTEXT-IDDISTR                 
171207     MOVE W41403-OBKR-IDKUNDNR   TO W4140A-OBTEXT-IDKUNDNR                
171307     MOVE W41403-OBKR-KDFRAKT    TO W4140A-OBTEXT-KDFRAKT                 
171407     MOVE W41403-OBKR-IDKUNDRF (1:7) TO W4140A-OBTEXT-IDORDNR             
171507     MOVE +2                     TO W4140A-OBTEXT-KDLIDEL                 
171607                                                                          
171707     MOVE W41403-OBKR-IDDC       TO W4140A-OBTEXT-IDDC                    
171807     MOVE W41403-OBKR-IDARTNR    TO W4140A-OBTEXT-IDARTNR                 
171907     MOVE W41403-OBKR-REKSIFFR   TO W4140A-OBTEXT-REKSIFFR                
172007     MOVE SPACE                  TO W4140A-OBTEXT-BEART                   
172107     MOVE W41403-OBKR-IDLOPNR    TO W4140A-OBTEXT-IDLOPNRE                
172207     MOVE W41403-OBKR-IDSEKVNR   TO W4140A-OBTEXT-IDKORTNR                
172307     MOVE W41403-OBKR-BERADREF   TO W4140A-OBTEXT-BERADREF                
172407     MOVE W41403-OBKR-IDKUNDRF-RO (1:7) TO W4140A-OBTEXT-IDRONR           
172507     MOVE ZERO                   TO W4140A-OBTEXT-TIRODAT                 
172607     MOVE W41403-OBKR-BEVOLREF   TO W4140A-OBTEXT-BEVOLREF                
172707     MOVE W41403-OBKR-KDORDBEK   TO W4140A-OBTEXT-KDRESTR                 
172807     MOVE WS-RED-KDERS           TO W4140A-OBTEXT-KDERS                   
172907     MOVE W41403-OBKR-KVBEART    TO W4140A-OBTEXT-KVBEART                 
173007     MOVE W41403-OBKR-BEERS      TO W4140A-OBTEXT-BEERS                   
173107                                                                          
173207     IF  W41403-OBKR-KDERS <  10                                          
173307     OR  W41403-OBKR-KDERS =  17 OR 19 OR 27 OR 28 OR 29 OR 52            
173407       MOVE ZERO                 TO W4140A-OBTEXT-KDERSUP                 
173507     ELSE                                                                 
173607         MOVE +1                 TO W4140A-OBTEXT-KDERSUP                 
173707     END-IF                                                               
173807                                                                          
173907     MOVE W41403-OBKR-KDKVBRYT   TO W4140A-OBTEXT-KDKVBRYT                
174007     MOVE W41403-OBKR-KDDSP      TO W4140A-OBTEXT-KDDSP                   
174107     MOVE W41403-OBKR-KDFAKTYP   TO W4140A-OBTEXT-KDFAKTYP                
174207     MOVE ZERO                   TO W4140A-OBTEXT-KDRO                    
174307                                                                          
174407     PERFORM S25-SKRIV-W4140A-005                                         
174507     .                                                                    
174607     EJECT                                                                
174707 DS-VR-SUD SECTION.                                                       
174807                                                                          
174907     MOVE SPACE                  TO SUD-AREA                              
175007                                                                          
175107     MOVE 'SUD'                  TO SUD-IDPTYP                            
175207                                                                          
175307     MOVE W41403-OBKR-IDDISTR    TO SUD-IDDISTR                           
175407     MOVE W41403-OBKR-IDKUNDNR   TO SUD-IDKUNDNR                          
175507     MOVE WS-RED-IDSUPPL-SU      TO SUD-IDSUPPL                           
175607                                                                          
175707     MOVE W41403-OBKR-IDKUNDRF (1:7) TO SUD-IDORDNR-002                   
175807     MOVE W41403-OBKR-IDARTNR    TO SUD-IDARTNR                           
175907     MOVE W41403-OBKR-REKSIFFR   TO SUD-REKSIFFR                          
176007     IF W41403-OBKR-KDORDBEK = 43 OR 44                                   
176107       MOVE W41403-OBKR-KVBEART-Q TO SUD-KVBEART-002                      
176207     ELSE                                                                 
176307       MOVE ZERO                  TO SUD-KVBEART-002                      
176407     END-IF                                                               
176507     MOVE W41403-OBKR-TITPO      TO SUD-TITPO                             
176607     MOVE W41403-OBKR-KDTPOTYP   TO SUD-KDTPOTYP                          
176707                                                                          
176807     MOVE WS-RED-KDVRTPO         TO SUD-KDVRTPO                           
176907                                                                          
177007     MOVE W41403-OBKR-KDVRINFO   TO SUD-KDVRINFO                          
177107                                                                          
177207     MOVE W41403-OBKR-TIREGDAT   TO SUD-TIAAMMDD                          
177307     MOVE W41403-OBKR-TIKLOCK    TO SUD-TIKLOCK                           
177407                                                                          
177507     PERFORM S12-SKRIV-W41406-SUD                                         
177607     .                                                                    
177707     EJECT                                                                
177807 DT-BASLAGER SECTION.                                                     
177907                                                                          
178007     MOVE SPACE                  TO W4140E-BASL-W414007                   
178107                                                                          
178207     MOVE W41403-OBKR-IDARTNR    TO W4140E-BASL-IDARTNR                   
178307     MOVE W41403-OBKR-IDDISTR    TO W4140E-BASL-IDDISTR                   
178407     MOVE W41403-OBKR-IDKUNDNR   TO W4140E-BASL-IDKUNDNR                  
178507     MOVE W41403-OBKR-IDKUNDRF   TO W4140E-BASL-IDKUNDRF                  
178607     MOVE W41403-OBKR-KDORDBEK   TO W4140E-BASL-KDORDBEK                  
178707     MOVE W41403-OBKR-KVBEART    TO W4140E-BASL-KVBEART-Q                 
178807     MOVE W41403-OBKR-TITPO      TO W4140E-BASL-TITPO                     
178907                                                                          
179007     PERFORM S34-SKRIV-W4140E                                             
179107     .                                                                    
179207     EJECT                                                                
179307 DU-SPX-0X3-OBKR   SECTION.                                               
179407                                                                          
179507     MOVE SPACE                  TO W414X3-AREA                           
179607                                                                          
179707     MOVE '0X3'                    TO W414X3-0X3-IDPTYP                   
179807     MOVE W41403-OBKR-IDSYSTEM     TO W414X3-0X3-IDSYSTEM                 
179907     MOVE W41403-OBKR-IDDISTR      TO W414X3-0X3-IDDISTR                  
180007     MOVE W41403-OBKR-IDKUNDNR     TO W414X3-0X3-IDKUNDNR                 
180107     MOVE W41403-OBKR-IDKUNDRF     TO W414X3-0X3-IDKUNDRF                 
180207     MOVE W41403-OBKR-IDDC         TO W414X3-0X3-IDDC                     
180307     MOVE W41403-OBKR-IDARTNR      TO W414X3-0X3-IDARTNR                  
180407     MOVE W41403-OBKR-KVBEART      TO W414X3-0X3-KVBEART                  
180507     MOVE W41403-OBKR-KVBEART-Q    TO W414X3-0X3-KVBEART-Q                
180607     MOVE W41403-OBKR-BERADREF     TO W414X3-0X3-BERADREF                 
180707     MOVE W41403-OBKR-KDORDBEK     TO W414X3-0X3-KDORDBEK                 
180807     MOVE W41403-OBKR-IDKUNDRF-RO  TO W414X3-0X3-IDKUNDRF-RO              
180907     MOVE W41403-OBKR-KDERS        TO W414X3-0X3-KDERS                    
181007     MOVE W41403-OBKR-FLTILLK      TO W414X3-0X3-FLTILLK                  
181107     MOVE W41403-OBKR-IDARTNR-TILLK TO W414X3-0X3-IDARTNR-TILLK           
181207                                                                          
181307     PERFORM S41-SKRIV-W414X3                                             
181407     .                                                                    
181507     EJECT                                                                
181607 DX-INIT-OBKR SECTION.                                                    
181707                                                                          
181807*    -- REDIGERA IDDISTR & IDSUPPL MHT FTAG & KUNDSTRUKTUR                
181907     MOVE W41403-OBKR-IDDC       TO WS-IDDC                               
182007     PERFORM S51-RED-IDSUPPL-SU                                           
182107     MOVE WS-SU-IDSUPPL-UT       TO WS-RED-IDSUPPL-SU                     
182207                                                                          
182307*    -- REDIGERA KVBEART & KVBEART-Q MHT KDORDBEK                         
182407     IF  W41403-OBKR-KDORDBEK = 92                                        
182507       COMPUTE WS-RED-KVBEART-OBK92 = W41403-OBKR-KVPREAVB                
182607                                    + W41403-OBKR-KVPRERO                 
182707       MOVE W41403-OBKR-KVPREAVB TO WS-RED-KVBEART-Q-OBK92                
182807     ELSE                                                                 
182907       MOVE W41403-OBKR-KVBEART  TO WS-RED-KVBEART-OBK92                  
183007       MOVE W41403-OBKR-KVBEART-Q TO WS-RED-KVBEART-Q-OBK92               
183107     END-IF                                                               
183207                                                                          
183307*    -- REDIGERA KDVRTPO MHT KDTPOTYP & IDSYSTEM                          
183407     IF  W41403-OBKR-KDTPOTYP = +1                                        
183507       IF  W41403-OBKR-IDSYSTEM = 'VR  '                                  
183607         MOVE +1                 TO WS-RED-KDVRTPO                        
183707       ELSE                                                               
183807         MOVE +2                 TO WS-RED-KDVRTPO                        
183907       END-IF                                                             
184007     ELSE                                                                 
184107       MOVE +0                   TO WS-RED-KDVRTPO                        
184207     END-IF                                                               
184307                                                                          
184407*    -- REDIGERA KDERS                                                    
184507     MOVE W41403-OBKR-KDERS      TO WS-RED-KDERS                          
184607     IF W41403-OBKR-KDERS > ZERO AND < 10                                 
184707        ADD +10                  TO WS-RED-KDERS                          
184807     END-IF                                                               
184907     .                                                                    
185007     EJECT                                                                
185107 E-BEH-TILLTPO SECTION.                                                   
185207                                                                          
185307     PERFORM EX-INIT-RADEN                                                
185407                                                                          
185507     IF W41403-TILLTPO-FLTILLK = NEJ                                      
185607       IF  W41403-TILLTPO-IDSYSTEM NOT = 'VR'                             
185707                  AND (NOT DIST24-NORGE)                                  
185807           PERFORM EH-VR-SU1                                              
185907       END-IF                                                             
186007                                                                          
186107       MOVE W41403-TILLTPO-IDDISTR TO TEST-IDDISTR2                       
186207                                                                          
186307       IF  W41403-TILLTPO-IDSYSTEM NOT = 'VIPS'                           
186407       AND W41403-TILLTPO-IDSYSTEM NOT = 'VDI'                            
186507         IF  DIST98-EJ-ONORDER                                            
186607         AND W41403-TILLTPO-IDSYSTEM   = 'LDC'                            
186707           CONTINUE                                                       
186807         ELSE                                                             
186907           PERFORM EI-NOAC-015-BYPASS                                     
187007           PERFORM EJ-NOAC-020-ONORD                                      
187107         END-IF                                                           
187207       END-IF                                                             
187307     END-IF                                                               
187407                                                                          
187507     IF W41403-TILLTPO-KVBEART NOT = W41403-TILLTPO-KVBEART-Q             
187607        IF NOT DIST24-NORGE                                               
187707          PERFORM EK-VR-SU3                                               
187807        END-IF                                                            
187907        PERFORM EL-NOAC-006-OBKVAN                                        
188007     END-IF                                                               
188107                                                                          
188207     IF NOT DIST24-NORGE                                                  
188307       PERFORM EC-VR-SU4                                                  
188407       PERFORM ED-VR-SU5                                                  
188507     END-IF                                                               
188607     PERFORM EE-NOAC-007                                                  
188707     .                                                                    
188807     EJECT                                                                
188907 EC-VR-SU4 SECTION.                                                       
189007                                                                          
189107     MOVE SPACE                  TO SU4-AREA                              
189207                                                                          
189307     MOVE 'SU4'                  TO SU4-IDPTYP                            
189407                                                                          
189507     MOVE W41403-TILLTPO-IDDISTR TO SU4-IDDISTR                           
189607     MOVE W41403-TILLTPO-IDKUNDNR TO SU4-IDKUNDNR                         
189707     MOVE WS-RED-IDSUPPL-SU      TO SU4-IDSUPPL                           
189807                                                                          
189907     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO SU4-IDORDNR-002                
190007     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO SU4-IDORDNR7-LEV               
190107                                                                          
190207     MOVE W41403-TILLTPO-IDARTNR    TO SU4-IDARTNR                        
190307     MOVE W41403-TILLTPO-REKSIFFR   TO SU4-REKSIFFR                       
190407     MOVE W41403-TILLTPO-KDFAKTYP   TO SU4-KDFAKTYP                       
190507     MOVE ZERO                      TO SU4-KDRO                           
190607     MOVE W41403-TILLTPO-KDORDKL    TO SU4-KDORDER                        
190707     MOVE NEJ                       TO SU4-TID-ERS                        
190807     MOVE W41403-TILLTPO-KDVRINFO   TO SU4-KDVRINFO                       
190907     MOVE W41403-TILLTPO-TITPO      TO SU4-TIDISPIN                       
191007     MOVE W41403-TILLTPO-KVBEART-Q  TO SU4-KVRO-002                       
191107     MOVE W41403-TILLTPO-KDORDBEK   TO SU4-KDRESTR                        
191207     MOVE W41403-TILLTPO-TIREGDAT   TO SU4-TIAAMMDD-REG                   
191307     MOVE W41403-TILLTPO-TIKLOCK    TO SU4-TIKLOCK-REG                    
191407     ADD +200                       TO SU4-TIKLOCK-REG                    
191507                                                                          
191607     PERFORM S29-SKRIV-W41406-SU4                                         
191707     .                                                                    
191807     EJECT                                                                
191907 ED-VR-SU5 SECTION.                                                       
192007                                                                          
192107     MOVE SPACE                  TO SU5-AREA                              
192207                                                                          
192307     MOVE 'SU5'                  TO SU5-IDPTYP                            
192407                                                                          
192507     MOVE W41403-TILLTPO-IDDISTR TO SU5-IDDISTR                           
192607     MOVE W41403-TILLTPO-IDKUNDNR TO SU5-IDKUNDNR                         
192707     MOVE WS-RED-IDSUPPL-SU      TO SU5-IDSUPPL                           
192807                                                                          
192907     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO SU5-IDRONR                     
193007                                                                          
193107     MOVE W41403-TILLTPO-IDARTNR    TO SU5-IDARTNR                        
193207     MOVE W41403-TILLTPO-REKSIFFR   TO SU5-REKSIFFR                       
193307     MOVE W41403-TILLTPO-KVBEART-Q  TO SU5-KVRO                           
193407     MOVE W41403-TILLTPO-KDORDKL    TO SU5-KDORDER                        
193507     MOVE W41403-TILLTPO-KDFAKTYP   TO SU5-KDFAKTYP                       
193607     MOVE W41403-TILLTPO-KDORDKL    TO SU5-KDORDKL                        
193707     MOVE W41403-TILLTPO-KDVRINFO   TO SU5-KDVRINFO                       
193807     MOVE W41403-TILLTPO-TIREGDAT   TO SU5-TIAAMMDD-REG                   
193907     MOVE W41403-TILLTPO-TIKLOCK    TO SU5-TIKLOCK-REG                    
194007     ADD +300                       TO SU5-TIKLOCK-REG                    
194107                                                                          
194207     PERFORM S30-SKRIV-W41406-SU5                                         
194307     .                                                                    
194407     EJECT                                                                
194507 EE-NOAC-007 SECTION.                                                     
194607                                                                          
194707     MOVE SPACE                  TO W4140A-OBLAG-W461007                  
194807                                                                          
194907     MOVE '007'                  TO W4140A-OBLAG-IDPTYP                   
195007                                                                          
195107     MOVE W41403-TILLTPO-IDDISTR    TO W4140A-OBLAG-IDDISTR               
195207     MOVE W41403-TILLTPO-IDKUNDNR   TO W4140A-OBLAG-IDKUNDNR              
195307     MOVE W41403-TILLTPO-KDFRAKT    TO W4140A-OBLAG-KDFRAKT               
195407     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO W4140A-OBLAG-IDORDNR           
195507     MOVE ZERO                      TO W4140A-OBLAG-KDLIDEL               
195607     MOVE WC-CDC-SE                 TO W4140A-OBLAG-IDDC                  
195707     MOVE W41403-TILLTPO-IDARTNR    TO W4140A-OBLAG-IDARTNR               
195807     MOVE W41403-TILLTPO-REKSIFFR   TO W4140A-OBLAG-REKSIFFR              
195907     MOVE SPACE                     TO W4140A-OBLAG-BEART                 
196007     MOVE W41403-TILLTPO-BERADREF   TO W4140A-OBLAG-BERADREF              
196107     MOVE W41403-TILLTPO-BEVOLREF   TO W4140A-OBLAG-BEVOLREF              
196207                                                                          
196307     MOVE W41403-TILLTPO-IDKUNDRF(1:7)                                    
196407                                    TO W4140A-OBLAG-IDRONR                
196507     MOVE ZERO                      TO W4140A-OBLAG-TIRODAT               
196607     MOVE W41403-TILLTPO-TIREGDAT-TPO TO W4140A-OBLAG-TIORDREG            
196707     MOVE W41403-TILLTPO-KDORDBEK   TO W4140A-OBLAG-KDRESTR               
196807     MOVE W41403-TILLTPO-KVBEART-Q  TO W4140A-OBLAG-KVBEART               
196907     MOVE ZERO                      TO W4140A-OBLAG-KVAVBART              
197007     MOVE W41403-TILLTPO-KDDSP      TO W4140A-OBLAG-KDDSP                 
197107     MOVE W41403-TILLTPO-KVBEART-Q  TO W4140A-OBLAG-KVRO                  
197207     MOVE W41403-TILLTPO-KDFAKTYP   TO W4140A-OBLAG-KDFAKTYP              
197307     MOVE W41403-TILLTPO-TIDISPIN   TO W4140A-OBLAG-TIDISPIN              
197407     MOVE W41403-TILLTPO-KDKVBRYT   TO W4140A-OBLAG-KDKVBRYT              
197507     MOVE W41403-TILLTPO-TIREGDAT   TO W4140A-OBLAG-TIAAMMDD              
197607     MOVE W41403-TILLTPO-TIKLOCK    TO W4140A-OBLAG-TIKLOCK               
197707                                                                          
197807     PERFORM S27-SKRIV-W4140A-007                                         
197907     .                                                                    
198007     EJECT                                                                
198107 EH-VR-SU1 SECTION.                                                       
198207                                                                          
198307     MOVE SPACE                     TO SU1-AREA                           
198407                                                                          
198507     MOVE 'SU1'                     TO SU1-IDPTYP                         
198607                                                                          
198707     MOVE W41403-TILLTPO-IDDISTR    TO SU1-IDDISTR                        
198807     MOVE W41403-TILLTPO-IDKUNDNR   TO SU1-IDKUNDNR                       
198907     MOVE WS-RED-IDSUPPL-SU         TO SU1-IDSUPPL                        
199007                                                                          
199107     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO SU1-IDORDNR-002                
199207                                                                          
199307     MOVE W41403-TILLTPO-IDARTNR    TO SU1-IDARTNR                        
199407     MOVE W41403-TILLTPO-REKSIFFR   TO SU1-REKSIFFR                       
199507     MOVE W41403-TILLTPO-KVBEART    TO SU1-KVBEART-002                    
199607     MOVE W41403-TILLTPO-KDORDKL    TO SU1-KDORDER                        
199707     MOVE ZERO                      TO SU1-KDTPOTYP                       
199807     MOVE W41403-TILLTPO-KDFAKTYP   TO SU1-KDFAKTYP                       
199907                                                                          
200007     IF  W41403-TILLTPO-KDPRTYP = 'P'                                     
200107       MOVE 'M'                     TO SU1-KDMANPR                        
200207     ELSE                                                                 
200307       MOVE W41403-TILLTPO-FLPRTILL TO SU1-KDMANPR                        
200407     END-IF                                                               
200507                                                                          
200607     MOVE WS-RED-KDVRTPO            TO SU1-KDVRTPO                        
200707                                                                          
200807     MOVE W41403-TILLTPO-KDVRINFO   TO SU1-KDVRINFO                       
200907                                                                          
201007     MOVE W41403-TILLTPO-TIREGDAT   TO SU1-TIAAMMDD-REG                   
201107     MOVE W41403-TILLTPO-TIKLOCK    TO SU1-TIKLOCK-REG                    
201207                                                                          
201307     PERFORM S13-SKRIV-W41406-SU1                                         
201407     .                                                                    
201507     EJECT                                                                
201607 EI-NOAC-015-BYPASS SECTION.                                              
201707                                                                          
201807     MOVE SPACE                     TO W41408-AREA                        
201907                                                                          
202007     MOVE '015'                     TO W41408-BYPASS-IDPTYP               
202107                                                                          
202207     MOVE W41403-TILLTPO-IDDISTR    TO W41408-BYPASS-IDDISTR              
202307     MOVE W41403-TILLTPO-IDKUNDNR   TO W41408-BYPASS-IDKUNDNR             
202407                                                                          
202507     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO  W41408-BYPASS-IDORDNR         
202607                                                                          
202707     MOVE W41403-TILLTPO-KDORDKL    TO W41408-BYPASS-KDORDKL              
202807     MOVE W41403-TILLTPO-IDARTNR    TO W41408-BYPASS-IDARTNR              
202907     MOVE W41403-TILLTPO-REKSIFFR   TO W41408-BYPASS-REKSIFFR             
203007     MOVE W41403-TILLTPO-BERADREF   TO W41408-BYPASS-BERADREF             
203107     MOVE W41403-TILLTPO-BEVOLREF   TO W41408-BYPASS-BEVOLREF             
203207     MOVE W41403-TILLTPO-KVBEART    TO W41408-BYPASS-KVBEART              
203307     MOVE W41403-TILLTPO-KDFAKTYP   TO W41408-BYPASS-KDFAKTYP             
203407     MOVE W41403-TILLTPO-KDDSP      TO W41408-BYPASS-KDDSP                
203507     MOVE 'N'                       TO W41408-BYPASS-FLABON               
203607                                                                          
203707     MOVE W41403-TILLTPO-KDTPOTYP   TO W41408-BYPASS-KDTPOTYP             
203807                                                                          
203907     PERFORM S18-SKRIV-W41408                                             
204007     .                                                                    
204107     EJECT                                                                
204207 EJ-NOAC-020-ONORD SECTION.                                               
204307                                                                          
204407     MOVE SPACE                     TO W41409-AREA                        
204507                                                                          
204607     MOVE '020'                     TO W41409-ONORD-IDPTYP                
204707                                                                          
204807     MOVE W41403-TILLTPO-IDDISTR    TO W41409-ONORD-IDDISTR               
204907     MOVE W41403-TILLTPO-IDKUNDNR   TO W41409-ONORD-IDKUNDNR              
205007                                                                          
205107     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO                                
205207                                            W41409-ONORD-IDORDNR          
205307                                                                          
205407     MOVE WC-CDC-SE                 TO W41409-ONORD-IDDC                  
205507     MOVE W41403-TILLTPO-KDORDKL    TO W41409-ONORD-KDORDKL               
205607     MOVE W41403-TILLTPO-BEVARREF   TO W41409-ONORD-BEVARREF              
205707     MOVE W41403-TILLTPO-BEVOLREF   TO W41409-ONORD-BEVOLREF              
205807     MOVE W41403-TILLTPO-BERADREF   TO W41409-ONORD-BERADREF              
205907     MOVE W41403-TILLTPO-IDARTNR    TO W41409-ONORD-IDARTNR               
206007     MOVE W41403-TILLTPO-REKSIFFR   TO W41409-ONORD-REKSIFFR              
206107     MOVE W41403-TILLTPO-KVBEART    TO W41409-ONORD-KVBEART               
206207     MOVE W41403-TILLTPO-TIREGDAT-TPO TO W41409-ONORD-TIORDREG            
206307     MOVE ZERO                      TO W41409-ONORD-IDRONR                
206407     MOVE ZERO                      TO W41409-ONORD-TIRODAT               
206507     MOVE W41403-TILLTPO-PRARTNTO   TO W41409-ONORD-PRARTNTO              
206607     MOVE W41403-TILLTPO-PRARTBTO-EXP TO W41409-ONORD-PRARTBTO-EXP        
206707     MOVE W41403-TILLTPO-KDPRODSL   TO W41409-ONORD-KDPRODSL              
206807     MOVE W41403-TILLTPO-IDFKNGRP   TO W41409-ONORD-IDFKNGRP              
206907     MOVE W41403-TILLTPO-FLINVEST   TO W41409-ONORD-FLINVEST              
207007     MOVE W41403-TILLTPO-KDDSP      TO W41409-ONORD-KDDSP                 
207107                                                                          
207207     IF  W41403-TILLTPO-KDPRTYP = 'P'                                     
207307       MOVE 'M'                     TO W41409-ONORD-KDMANPR               
207407     ELSE                                                                 
207507       MOVE W41403-TILLTPO-FLPRTILL TO W41409-ONORD-KDMANPR               
207607     END-IF                                                               
207707                                                                          
207807     MOVE 'N'                       TO W41409-ONORD-FLABON                
207907                                                                          
208007     MOVE W41403-TILLTPO-KDTPOTYP   TO W41409-ONORD-KDTPOTYP              
208107                                                                          
208207     PERFORM S19-SKRIV-W41409                                             
208307     .                                                                    
208407     EJECT                                                                
208507 EK-VR-SU3 SECTION.                                                       
208607                                                                          
208707     MOVE SPACE                     TO SU3-AREA                           
208807                                                                          
208907     MOVE 'SU3'                     TO SU3-IDPTYP                         
209007                                                                          
209107     COMPUTE SU3-KVBEART-002 = W41403-TILLTPO-KVBEART-Q                   
209207                         - W41403-TILLTPO-KVBEART                         
209307     END-COMPUTE                                                          
209407     IF W41403-TILLTPO-KVBEART-Q > W41403-TILLTPO-KVBEART                 
209507        MOVE 1                      TO SU3-KDKVFOR                        
209607        MOVE 43                     TO SU3-KDRESTR                        
209707     ELSE                                                                 
209807        MOVE 2                      TO SU3-KDKVFOR                        
209907        MOVE 44                     TO SU3-KDRESTR                        
210007     END-IF                                                               
210107     MOVE W41403-TILLTPO-IDDISTR    TO SU3-IDDISTR                        
210207     MOVE W41403-TILLTPO-IDKUNDNR   TO SU3-IDKUNDNR                       
210307     MOVE WS-RED-IDSUPPL-SU         TO SU3-IDSUPPL                        
210407                                                                          
210507     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO SU3-IDORDNR-002                
210607     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO SU3-IDORDNR7-LEV               
210707                                                                          
210807     MOVE W41403-TILLTPO-IDARTNR    TO SU3-IDARTNR                        
210907     MOVE W41403-TILLTPO-REKSIFFR   TO SU3-REKSIFFR                       
211007     MOVE W41403-TILLTPO-KDFAKTYP   TO SU3-KDFAKTYP                       
211107     MOVE ZERO                      TO SU3-KDRO                           
211207     MOVE W41403-TILLTPO-KDORDKL    TO SU3-KDORDER                        
211307     MOVE W41403-TILLTPO-KVQPACK-1  TO SU3-KVQPACK-1                      
211407     MOVE NEJ                       TO SU3-TID-ERS                        
211507     MOVE W41403-TILLTPO-KDORDKL    TO SU3-KDORDKL                        
211607     MOVE ZERO                      TO SU3-FIKTIV-KVANT                   
211707                                                                          
211807     MOVE WS-RED-KDVRTPO            TO SU3-KDVRTPO                        
211907                                                                          
212007     MOVE W41403-TILLTPO-KDVRINFO   TO SU3-KDVRINFO                       
212107                                                                          
212207     MOVE W41403-TILLTPO-TIREGDAT   TO SU3-TIAAMMDD                       
212307     MOVE W41403-TILLTPO-TIKLOCK    TO SU3-TIKLOCK                        
212407     ADD +100                       TO SU3-TIKLOCK                        
212507                                                                          
212607     PERFORM S15-SKRIV-W41406-SU3                                         
212707     .                                                                    
212807     EJECT                                                                
212907 EL-NOAC-006-OBKVAN SECTION.                                              
213007                                                                          
213107     MOVE SPACE                     TO W4140A-OBKVAN-W461006              
213207                                                                          
213307     MOVE '006'                     TO W4140A-OBKVAN-IDPTYP               
213407                                                                          
213507     MOVE W41403-TILLTPO-IDDISTR    TO W4140A-OBKVAN-IDDISTR              
213607     MOVE W41403-TILLTPO-IDKUNDNR   TO W4140A-OBKVAN-IDKUNDNR             
213707     MOVE W41403-TILLTPO-KDFRAKT    TO W4140A-OBKVAN-KDFRAKT              
213807     MOVE W41403-TILLTPO-IDKUNDRF (1:7) TO W4140A-OBKVAN-IDORDNR          
213907     MOVE ZERO                      TO W4140A-OBKVAN-KDLIDEL              
214007     MOVE WC-CDC-SE                 TO W4140A-OBKVAN-IDDC                 
214107     MOVE W41403-TILLTPO-IDARTNR    TO W4140A-OBKVAN-IDARTNR              
214207     MOVE W41403-TILLTPO-REKSIFFR   TO W4140A-OBKVAN-REKSIFFR             
214307     MOVE SPACE                     TO W4140A-OBKVAN-BEART                
214407     MOVE W41403-TILLTPO-BERADREF   TO W4140A-OBKVAN-BERADREF             
214507                                                                          
214607     MOVE W41403-TILLTPO-IDKUNDRF (1:7)                                   
214707                                    TO W4140A-OBKVAN-IDRONR               
214807                                                                          
214907     MOVE ZERO                      TO W4140A-OBKVAN-TIRODAT              
215007     MOVE W41403-TILLTPO-BEVOLREF   TO W4140A-OBKVAN-BEVOLREF             
215107     IF W41403-TILLTPO-KVBEART-Q > W41403-TILLTPO-KVBEART                 
215207        MOVE 43                     TO W4140A-OBKVAN-KDRESTR              
215307     ELSE                                                                 
215407        MOVE 44                     TO W4140A-OBKVAN-KDRESTR              
215507     END-IF                                                               
215607                                                                          
215707     MOVE W41403-TILLTPO-KVBEART    TO W4140A-OBKVAN-KVBEART              
215807     MOVE W41403-TILLTPO-KVBEART-Q  TO W4140A-OBKVAN-KVBEART-Q            
215907                                                                          
216007     MOVE W41403-TILLTPO-KVQPACK-1  TO W4140A-OBKVAN-KVQPACK-1            
216107     MOVE W41403-TILLTPO-KDKVBRYT   TO W4140A-OBKVAN-KDKVBRYT             
216207     MOVE W41403-TILLTPO-KDDSP      TO W4140A-OBKVAN-KDDSP                
216307     MOVE W41403-TILLTPO-KDFAKTYP   TO W4140A-OBKVAN-KDFAKTYP             
216407     MOVE W41403-TILLTPO-TIREGDAT   TO W4140A-OBKVAN-TIAAMMDD             
216507     MOVE W41403-TILLTPO-TIKLOCK    TO W4140A-OBKVAN-TIKLOCK              
216607                                                                          
216707     PERFORM S26-SKRIV-W4140A-006                                         
216807     .                                                                    
216907     EJECT                                                                
217007 EX-INIT-RADEN SECTION.                                                   
217107                                                                          
217207*    -- REDIGERA IDSUPPL MHT FTAG & KUNDSTRUKTUR                          
217307     MOVE WC-CDC-SE                 TO WS-IDDC                            
217407     PERFORM S51-RED-IDSUPPL-SU                                           
217507     MOVE WS-SU-IDSUPPL-UT       TO WS-RED-IDSUPPL-SU                     
217607                                                                          
217707*    -- REDIGERA KDVRTPO MHT KDTPOTYP & IDSYSTEM                          
217807     IF  W41403-TILLTPO-KDTPOTYP = +1                                     
217907       IF  W41403-TILLTPO-IDSYSTEM = 'VR  '                               
218007         MOVE +1                 TO WS-RED-KDVRTPO                        
218107       ELSE                                                               
218207         MOVE +2                 TO WS-RED-KDVRTPO                        
218307       END-IF                                                             
218407     ELSE                                                                 
218507       MOVE +0                   TO WS-RED-KDVRTPO                        
218607     END-IF                                                               
218707                                                                          
218807*    -- TITPO OMVANDLAS TILL AVV                                          
218907     MOVE W41403-TILLTPO-TITPO   TO DAT-I-TIDATUM                         
219007     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
219107     PERFORM S50-CALL-WDATKONV                                            
219207     MOVE DAT-TIAAVV-GRP         TO TITPO-TIAAVV                          
219307     MOVE DAT-TIAAP              TO TITPO-TIAAP                           
219407     .                                                                    
219507     EJECT                                                                
219607 F-BEH-REGTPO SECTION.                                                    
219707                                                                          
219807     PERFORM FX-INIT-RADEN                                                
219907                                                                          
220007     IF W41405-REGTPO-IDSYSTEM NOT = 'VR' AND (NOT DIST24-NORGE)          
220107        PERFORM FB-VR-SUD                                                 
220207     END-IF                                                               
220307     .                                                                    
220407     EJECT                                                                
220507 FB-VR-SUD SECTION.                                                       
220607                                                                          
220707     MOVE SPACE                  TO SUD-AREA                              
220807                                                                          
220907     MOVE 'SUD'                  TO SUD-IDPTYP                            
221007                                                                          
221107     MOVE W41405-REGTPO-IDDISTR  TO SUD-IDDISTR                           
221207     MOVE W41405-REGTPO-IDKUNDNR TO SUD-IDKUNDNR                          
221307     MOVE WS-RED-IDSUPPL-SU      TO SUD-IDSUPPL                           
221407                                                                          
221507     MOVE W41405-REGTPO-IDKUNDRF (1:7) TO SUD-IDORDNR-002                 
221607     MOVE W41405-REGTPO-IDARTNR    TO SUD-IDARTNR                         
221707     MOVE W41405-REGTPO-REKSIFFR   TO SUD-REKSIFFR                        
221807     MOVE W41405-REGTPO-KVBEART-Q  TO SUD-KVBEART-002                     
221907     MOVE W41405-REGTPO-TITPO      TO SUD-TITPO                           
222007     MOVE W41405-REGTPO-KDTPOTYP   TO SUD-KDTPOTYP                        
222107     MOVE W41405-REGTPO-KDVRINFO   TO SUD-KDVRINFO                        
222207     MOVE WS-RED-KDVRTPO           TO SUD-KDVRTPO                         
222307                                                                          
222407     MOVE W41405-REGTPO-TIREGDAT   TO SUD-TIAAMMDD                        
222507     MOVE W41405-REGTPO-TIKLOCK    TO SUD-TIKLOCK                         
222607                                                                          
222707     PERFORM S12-SKRIV-W41406-SUD                                         
222807     .                                                                    
222907     EJECT                                                                
223007 FX-INIT-RADEN SECTION.                                                   
223107                                                                          
223207*    -- REDIGERA IDDISTR & IDSUPPL MHT FTAG & KUNDSTRUKTUR                
223307     MOVE WC-CDC-SE                TO WS-IDDC                             
223407     PERFORM S51-RED-IDSUPPL-SU                                           
223507     MOVE WS-SU-IDSUPPL-UT       TO WS-RED-IDSUPPL-SU                     
223607                                                                          
223707*    -- REDIGERA KDVRTPO MHT KDTPOTYP & IDSYSTEM                          
223807     IF  W41405-REGTPO-KDTPOTYP = +1                                      
223907       IF  W41405-REGTPO-IDSYSTEM = 'VR  '                                
224007         MOVE +1                 TO WS-RED-KDVRTPO                        
224107       ELSE                                                               
224207         MOVE +2                 TO WS-RED-KDVRTPO                        
224307       END-IF                                                             
224407     ELSE                                                                 
224507       MOVE +0                   TO WS-RED-KDVRTPO                        
224607     END-IF                                                               
224707                                                                          
224807*    -- TITPO OMVANDLAS TILL AVV                                          
224907     MOVE W41405-REGTPO-TITPO   TO DAT-I-TIDATUM                          
225007     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
225107     PERFORM S50-CALL-WDATKONV                                            
225207     MOVE DAT-TIAAVV-GRP         TO TITPO-TIAAVV                          
225307     MOVE DAT-TIAAP              TO TITPO-TIAAP                           
225407     .                                                                    
225507     EJECT                                                                
225607 G-BEH-ANNVOR SECTION.                                                    
225707                                                                          
225807     MOVE SPACE                   TO ANNVOR-AREA                          
225907                                                                          
226007     MOVE 'ANN'                   TO ANNVOR-IDPTYP                        
226107                                                                          
226207     MOVE W41403-ANNVOR-IDARTNR   TO ANNVOR-IDARTNR                       
226307     MOVE W41403-ANNVOR-IDDC      TO ANNVOR-IDDC                          
226407     MOVE W41403-ANNVOR-IDDISTR   TO ANNVOR-IDDISTR                       
226507     MOVE W41403-ANNVOR-IDKUNDNR  TO ANNVOR-IDKUNDNR                      
226607     MOVE W41403-ANNVOR-KDPRODSL  TO ANNVOR-KDPRODSL                      
226707     MOVE W41403-ANNVOR-KVANNANT  TO ANNVOR-KVANNANT                      
226807     MOVE W41403-ANNVOR-KVAVBART  TO ANNVOR-KVAVBART                      
226907     MOVE W41403-ANNVOR-KVBEART-Q TO ANNVOR-KVBEART-Q                     
227007     MOVE W41403-ANNVOR-FLDIRLEV  TO ANNVOR-FLDIRLEV                      
227107                                                                          
227207     PERFORM S33-SKRIV-W4140D                                             
227307     .                                                                    
227407     EJECT                                                                
227507 Z-FINIT SECTION.                                                         
227607     CLOSE W41401                                                         
227707           W41403                                                         
227807           W41405                                                         
227907           W41406                                                         
228007           W41407                                                         
228107           W41408                                                         
228207           W41409                                                         
228307           W4140A                                                         
228407           W4140J                                                         
228507           W4140D                                                         
228607           W4140E                                                         
228707           W4140G                                                         
228807           W4140I                                                         
228907           W414X2 W414X3                                                  
229010           W4140S                                                         
229107                                                                          
229207     MOVE 'S' TO POSTSUM-OPKOD                                            
229307     CALL POSTSUM USING POSTSUM-PARM                                      
229407     .                                                                    
229507     EJECT                                                                
229607 S01-LAES-W41401 SECTION.                                                 
229707     SKIP2                                                                
229807     READ W41401 INTO W41401-AREA                                         
229907     AT END                                                               
230007        SET END-OF-W41401 TO TRUE                                         
230107                                                                          
230207     NOT AT END                                                           
230307        MOVE 'W41401' TO POSTSUM-FDNAMN                                   
230407        MOVE 'W41403D1' TO POSTSUM-DDNAMN2                                
230507        MOVE W41401-OHUV-IDPTYP TO POSTSUM-TRANSTYP                       
230607        CALL POSTSUM USING POSTSUM-PARM                                   
230707     END-READ                                                             
230807     .                                                                    
230907     EJECT                                                                
231007 S02-LAES-W41403 SECTION.                                                 
231107     SKIP2                                                                
231207     READ W41403 INTO W41403-AREA                                         
231307     AT END                                                               
231407        SET END-OF-W41403 TO TRUE                                         
231507                                                                          
231607     NOT AT END                                                           
231707        MOVE 'W41403' TO POSTSUM-FDNAMN                                   
231807        MOVE 'W41403D2' TO POSTSUM-DDNAMN2                                
231907        MOVE W41403-IDPTYP TO POSTSUM-TRANSTYP                            
232007        CALL POSTSUM USING POSTSUM-PARM                                   
232107     END-READ                                                             
232207     .                                                                    
232307     EJECT                                                                
232407 S03-LAES-W41405 SECTION.                                                 
232507     SKIP2                                                                
232607     READ W41405 INTO W41405-AREA                                         
232707     AT END                                                               
232807        SET END-OF-W41405 TO TRUE                                         
232907                                                                          
233007     NOT AT END                                                           
233107        MOVE 'W41405' TO POSTSUM-FDNAMN                                   
233207        MOVE 'W41403DE' TO POSTSUM-DDNAMN2                                
233307        MOVE W41403-IDPTYP TO POSTSUM-TRANSTYP                            
233407        CALL POSTSUM USING POSTSUM-PARM                                   
233507     END-READ                                                             
233607     .                                                                    
233707     EJECT                                                                
233807 S11-SKRIV-W41406-SUA SECTION.                                            
233907                                                                          
234007     WRITE W41406-SUA-POST FROM SUA-AREA                                  
234107                                                                          
234207     MOVE SUA-IDPTYP TO POSTSUM-TRANSTYP                                  
234307     MOVE 'W41406' TO POSTSUM-FDNAMN                                      
234407     MOVE 'W41403D3' TO POSTSUM-DDNAMN2                                   
234507     CALL POSTSUM USING POSTSUM-PARM                                      
234607     .                                                                    
234707     SKIP2                                                                
234807 S12-SKRIV-W41406-SUD SECTION.                                            
234907                                                                          
235007     WRITE W41406-SUD-POST FROM SUD-AREA                                  
235107                                                                          
235207     MOVE SUD-IDPTYP TO POSTSUM-TRANSTYP                                  
235307     MOVE 'W41406' TO POSTSUM-FDNAMN                                      
235407     MOVE 'W41403D3' TO POSTSUM-DDNAMN2                                   
235507     CALL POSTSUM USING POSTSUM-PARM                                      
235607     .                                                                    
235707     SKIP2                                                                
235807 S13-SKRIV-W41406-SU1 SECTION.                                            
235907                                                                          
236007     WRITE W41406-SU1-POST FROM SU1-AREA                                  
236107                                                                          
236207     MOVE SU1-IDPTYP TO POSTSUM-TRANSTYP                                  
236307     MOVE 'W41406' TO POSTSUM-FDNAMN                                      
236407     MOVE 'W41403D3' TO POSTSUM-DDNAMN2                                   
236507     CALL POSTSUM USING POSTSUM-PARM                                      
236607     .                                                                    
236707     SKIP2                                                                
236807 S14-SKRIV-W41406-SU2 SECTION.                                            
236907                                                                          
237007     WRITE W41406-SU2-POST FROM SU2-AREA                                  
237107                                                                          
237207     MOVE SU2-IDPTYP TO POSTSUM-TRANSTYP                                  
237307     MOVE 'W41406' TO POSTSUM-FDNAMN                                      
237407     MOVE 'W41403D3' TO POSTSUM-DDNAMN2                                   
237507     CALL POSTSUM USING POSTSUM-PARM                                      
237607     .                                                                    
237707     SKIP2                                                                
237807 S15-SKRIV-W41406-SU3 SECTION.                                            
237907                                                                          
238007     WRITE W41406-SU3-POST FROM SU3-AREA                                  
238107                                                                          
238207     MOVE SU3-IDPTYP TO POSTSUM-TRANSTYP                                  
238307     MOVE 'W41406' TO POSTSUM-FDNAMN                                      
238407     MOVE 'W41403D3' TO POSTSUM-DDNAMN2                                   
238507     CALL POSTSUM USING POSTSUM-PARM                                      
238607     .                                                                    
238707     EJECT                                                                
238807 S17-SKRIV-W41407 SECTION.                                                
238907     SKIP2                                                                
239007     WRITE W41407-POST FROM W41407-AREA                                   
239107                                                                          
239207     MOVE W41407-BIP-IDPTYP TO POSTSUM-TRANSTYP                           
239307     MOVE 'W41407' TO POSTSUM-FDNAMN                                      
239407     MOVE 'W41403D4' TO POSTSUM-DDNAMN2                                   
239507     CALL POSTSUM USING POSTSUM-PARM                                      
239607     .                                                                    
239707     EJECT                                                                
239807 S18-SKRIV-W41408 SECTION.                                                
239907     SKIP2                                                                
240007     WRITE W41408-POST FROM W41408-AREA                                   
240107                                                                          
240207     MOVE W41408-BYPASS-IDPTYP TO POSTSUM-TRANSTYP                        
240307     MOVE 'W41408' TO POSTSUM-FDNAMN                                      
240407     MOVE 'W41403D5' TO POSTSUM-DDNAMN2                                   
240507     CALL POSTSUM USING POSTSUM-PARM                                      
240607     .                                                                    
240707     EJECT                                                                
240807 S19-SKRIV-W41409 SECTION.                                                
240907     SKIP2                                                                
241007     WRITE W41409-POST FROM W41409-AREA                                   
241107                                                                          
241207     MOVE W41409-ONORD-IDPTYP TO POSTSUM-TRANSTYP                         
241307     MOVE 'W41409' TO POSTSUM-FDNAMN                                      
241407     MOVE 'W41403D6' TO POSTSUM-DDNAMN2                                   
241507     CALL POSTSUM USING POSTSUM-PARM                                      
241607     .                                                                    
241707     EJECT                                                                
241807 S22-SKRIV-W4140A-002 SECTION.                                            
241907                                                                          
242007     WRITE W4140A-002-POST FROM W4140A-OBHUV-W461002                      
242107                                                                          
242207     MOVE W4140A-OBHUV-IDPTYP TO POSTSUM-TRANSTYP                         
242307     MOVE 'W4140A' TO POSTSUM-FDNAMN                                      
242407     MOVE 'W41403D7' TO POSTSUM-DDNAMN2                                   
242507     CALL POSTSUM USING POSTSUM-PARM                                      
242607     .                                                                    
242707     SKIP2                                                                
242807 S23-SKRIV-W4140A-003 SECTION.                                            
242907                                                                          
243007     WRITE W4140A-003-POST FROM W4140A-OBEN-W461003                       
243107                                                                          
243207     MOVE W4140A-OBEN-IDPTYP TO POSTSUM-TRANSTYP                          
243307     MOVE 'W4140A' TO POSTSUM-FDNAMN                                      
243407     MOVE 'W41403D7' TO POSTSUM-DDNAMN2                                   
243507     CALL POSTSUM USING POSTSUM-PARM                                      
243607     .                                                                    
243707     SKIP2                                                                
243807 S24-SKRIV-W4140A-004 SECTION.                                            
243907                                                                          
244007     WRITE W4140A-004-POST FROM W4140A-OBEEN-W461004                      
244107                                                                          
244207     MOVE W4140A-OBEEN-IDPTYP TO POSTSUM-TRANSTYP                         
244307     MOVE 'W4140A' TO POSTSUM-FDNAMN                                      
244407     MOVE 'W41403D7' TO POSTSUM-DDNAMN2                                   
244507     CALL POSTSUM USING POSTSUM-PARM                                      
244607     .                                                                    
244707     SKIP2                                                                
244807 S25-SKRIV-W4140A-005 SECTION.                                            
244907                                                                          
245007     WRITE W4140A-005-POST FROM W4140A-OBTEXT-W461005                     
245107                                                                          
245207     MOVE W4140A-OBTEXT-IDPTYP TO POSTSUM-TRANSTYP                        
245307     MOVE 'W4140A' TO POSTSUM-FDNAMN                                      
245407     MOVE 'W41403D7' TO POSTSUM-DDNAMN2                                   
245507     CALL POSTSUM USING POSTSUM-PARM                                      
245607     .                                                                    
245707     SKIP2                                                                
245807 S26-SKRIV-W4140A-006 SECTION.                                            
245907                                                                          
246007     WRITE W4140A-006-POST FROM W4140A-OBKVAN-W461006                     
246107                                                                          
246207     MOVE W4140A-OBKVAN-IDPTYP TO POSTSUM-TRANSTYP                        
246307     MOVE 'W4140A' TO POSTSUM-FDNAMN                                      
246407     MOVE 'W41403D7' TO POSTSUM-DDNAMN2                                   
246507     CALL POSTSUM USING POSTSUM-PARM                                      
246607     .                                                                    
246707     SKIP2                                                                
246807 S27-SKRIV-W4140A-007 SECTION.                                            
246907                                                                          
247007     WRITE W4140A-007-POST FROM W4140A-OBLAG-W461007                      
247107                                                                          
247207     MOVE W4140A-OBLAG-IDPTYP TO POSTSUM-TRANSTYP                         
247307     MOVE 'W4140A' TO POSTSUM-FDNAMN                                      
247407     MOVE 'W41403D7' TO POSTSUM-DDNAMN2                                   
247507     CALL POSTSUM USING POSTSUM-PARM                                      
247607     .                                                                    
247707     SKIP2                                                                
247807 S28-SKRIV-W4140A-008 SECTION.                                            
247907                                                                          
248007     WRITE W4140A-008-POST FROM W4140A-OBSTOP-W461008                     
248107                                                                          
248207     MOVE W4140A-OBSTOP-IDPTYP TO POSTSUM-TRANSTYP                        
248307     MOVE 'W4140A' TO POSTSUM-FDNAMN                                      
248407     MOVE 'W41403D7' TO POSTSUM-DDNAMN2                                   
248507     CALL POSTSUM USING POSTSUM-PARM                                      
248607     .                                                                    
248707     EJECT                                                                
248807 S29-SKRIV-W41406-SU4 SECTION.                                            
248907                                                                          
249007     WRITE W41406-SU4-POST FROM SU4-AREA                                  
249107                                                                          
249207     MOVE SU4-IDPTYP TO POSTSUM-TRANSTYP                                  
249307     MOVE 'W41406' TO POSTSUM-FDNAMN                                      
249407     MOVE 'W41403D3' TO POSTSUM-DDNAMN2                                   
249507     CALL POSTSUM USING POSTSUM-PARM                                      
249607     .                                                                    
249707     EJECT                                                                
249807 S30-SKRIV-W41406-SU5 SECTION.                                            
249907                                                                          
250007     WRITE W41406-SU5-POST FROM SU5-AREA                                  
250107                                                                          
250207     MOVE SU5-IDPTYP TO POSTSUM-TRANSTYP                                  
250307     MOVE 'W41406' TO POSTSUM-FDNAMN                                      
250407     MOVE 'W41403D3' TO POSTSUM-DDNAMN2                                   
250507     CALL POSTSUM USING POSTSUM-PARM                                      
250607     .                                                                    
250707     EJECT                                                                
250807 S32-SKRIV-W4140J SECTION.                                                
250907     SKIP2                                                                
251007     WRITE W4140J-POST FROM W4140J-AREA                                   
251107                                                                          
251207     MOVE W4140J-AMC-IDPTYP TO POSTSUM-TRANSTYP                           
251307     MOVE 'W4140J' TO POSTSUM-FDNAMN                                      
251407     MOVE 'W41403D9' TO POSTSUM-DDNAMN2                                   
251507     CALL POSTSUM USING POSTSUM-PARM                                      
251607     .                                                                    
251707     EJECT                                                                
251807 S33-SKRIV-W4140D SECTION.                                                
251907     SKIP2                                                                
252007     WRITE W4140D-POST FROM ANNVOR-AREA                                   
252107                                                                          
252207     MOVE ANNVOR-IDPTYP TO POSTSUM-TRANSTYP                               
252307     MOVE 'W4140D' TO POSTSUM-FDNAMN                                      
252407     MOVE 'W41403DA' TO POSTSUM-DDNAMN2                                   
252507     CALL POSTSUM USING POSTSUM-PARM                                      
252607     .                                                                    
252707     EJECT                                                                
252807 S34-SKRIV-W4140E SECTION.                                                
252907     SKIP2                                                                
253007     WRITE W4140E-POST FROM W4140E-AREA                                   
253107                                                                          
253207     MOVE 'BAS'         TO POSTSUM-TRANSTYP                               
253307     MOVE 'W4140E' TO POSTSUM-FDNAMN                                      
253407     MOVE 'W41403DB' TO POSTSUM-DDNAMN2                                   
253507     CALL POSTSUM USING POSTSUM-PARM                                      
253607     .                                                                    
253707     EJECT                                                                
253807 S36-SKRIV-W4140G SECTION.                                                
253907     SKIP2                                                                
254007     WRITE W4140G-POST FROM W4140G-AREA                                   
254107                                                                          
254207     MOVE W4140G-SERV-IDPTYP TO POSTSUM-TRANSTYP                          
254307     MOVE 'W4140G' TO POSTSUM-FDNAMN                                      
254407     MOVE 'W41403DD' TO POSTSUM-DDNAMN2                                   
254507     CALL POSTSUM USING POSTSUM-PARM                                      
254607     .                                                                    
254707     EJECT                                                                
254807 S38-SKRIV-W4140I SECTION.                                                
254907     SKIP2                                                                
255007     WRITE W4140I-POST FROM W4140I-AREA                                   
255107                                                                          
255207     MOVE 'VOR'    TO POSTSUM-TRANSTYP                                    
255307     MOVE 'W4140I' TO POSTSUM-FDNAMN                                      
255407     MOVE 'W41403DG' TO POSTSUM-DDNAMN2                                   
255507     CALL POSTSUM USING POSTSUM-PARM                                      
255607     .                                                                    
255707     EJECT                                                                
255807                                                                          
255907 S40-SKRIV-W414X2 SECTION.                                                
256007     SKIP2                                                                
256107     WRITE W414X2-POST FROM W414X2-AREA                                   
256207                                                                          
256307     MOVE 'SPX ORAD' TO POSTSUM-TRANSTYP                                  
256407     MOVE 'W414X2'   TO POSTSUM-FDNAMN                                    
256507     MOVE 'W41403DI' TO POSTSUM-DDNAMN2                                   
256607     CALL POSTSUM USING POSTSUM-PARM                                      
256707     .                                                                    
256807                                                                          
256907 S41-SKRIV-W414X3 SECTION.                                                
257007     SKIP2                                                                
257107     WRITE W414X3-POST FROM W414X3-AREA                                   
257207                                                                          
257307     MOVE 'SPX OBEK' TO POSTSUM-TRANSTYP                                  
257407     MOVE 'W414X3'   TO POSTSUM-FDNAMN                                    
257507     MOVE 'W41403DJ' TO POSTSUM-DDNAMN2                                   
257607     CALL POSTSUM USING POSTSUM-PARM                                      
257707     .                                                                    
257807     EJECT                                                                
257910 S42-SKRIV-W4140S SECTION.                                                
258010     SKIP2                                                                
258110     WRITE W4140S-POST FROM W4140S-AREA                                   
258210                                                                          
258310     MOVE 'SCR ORAD' TO POSTSUM-TRANSTYP                                  
258410     MOVE 'W4140S'   TO POSTSUM-FDNAMN                                    
258510     MOVE 'W41403DK' TO POSTSUM-DDNAMN2                                   
258610     CALL POSTSUM USING POSTSUM-PARM                                      
258710     .                                                                    
258810     EJECT                                                                
258910 S50-CALL-WDATKONV SECTION.                                               
259010     SKIP2                                                                
259110     CALL WDATKONV USING       DAT-KDDATFORM                              
259210                               DAT-I-TIDATUM                              
259310                               DAT-O-TIDATUM                              
259410                               DAT-KDSVAR                                 
259510     .                                                                    
259610     EJECT                                                                
259710 S51-RED-IDSUPPL-SU SECTION.                                              
259810                                                                          
259910*    REDIGERA IDSUPPL FÖR SU-POSTER                                       
260010                                                                          
260110     IF  CDC-SE                                                           
260210       MOVE 711                TO WS-SU-IDSUPPL-UT                        
260310     ELSE                                                                 
260410       MOVE 729                TO WS-SU-IDSUPPL-UT                        
260510     END-IF                                                               
261010     .                                                                    
270010     EJECT                                                                
