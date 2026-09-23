000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3715400.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   97/09/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET RÄKNAR UT SNITTETARBETSDAGAR                          
001100* DVS TIDEN DET TAR ATT GODKÄNNA EN BYTESRAPPORT                          
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- UNDERLAG TILL BERÄKNING AV SNITTUNDERLAGET                 
002403     SELECT W3714D                     ASSIGN TO W37154D1.                
002407     SKIP2                                                                
002408*          --- LISTOR TILL BYTES DC 11 91 41 42 43 51                     
002409*          --- LISTA 91 ÄR NU EN FIL /2006-02-10                          
002410     SELECT LISTA                      ASSIGN TO W37154D2.                
002420     SELECT LISTB                      ASSIGN TO W37154D3.                
002430     SELECT LISTC                      ASSIGN TO W37154D4.                
002440     SELECT LISTD                      ASSIGN TO W37154D5.                
002450     SELECT LISTE                      ASSIGN TO W37154D6.                
002460     SELECT LISTF                      ASSIGN TO W37154D7.                
002470     SELECT LISTG                      ASSIGN TO W37154D8.                
002480     SELECT LISTH                      ASSIGN TO W37154D9.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W3714D                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W3714D      -L.                                                
003007     SKIP3                                                                
003008 FD  LISTA                                                                
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011     SKIP2                                                                
003020 01  LISTAS                      PIC X(121).                              
003030     SKIP3                                                                
003040 FD  LISTB                                                                
003050     RECORDING       V                                                    
003060     BLOCK CONTAINS  0.                                                   
003070     SKIP2                                                                
003080 01  LISTBS                      PIC X(125).                              
003100     EJECT                                                                
003110     SKIP3                                                                
003120 FD  LISTC                                                                
003130     RECORDING       F                                                    
003140     BLOCK CONTAINS  0.                                                   
003150     SKIP2                                                                
003160 01  LISTCS                      PIC X(121).                              
003170     EJECT                                                                
003180 FD  LISTD                                                                
003190     RECORDING       F                                                    
003191     BLOCK CONTAINS  0.                                                   
003192     SKIP2                                                                
003193 01  LISTDS                      PIC X(121).                              
003194     EJECT                                                                
003195 FD  LISTE                                                                
003196     RECORDING       F                                                    
003197     BLOCK CONTAINS  0.                                                   
003198     SKIP2                                                                
003199 01  LISTES                      PIC X(121).                              
003200     EJECT                                                                
003201 FD  LISTF                                                                
003202     RECORDING       F                                                    
003203     BLOCK CONTAINS  0.                                                   
003204     SKIP2                                                                
003205 01  LISTFS                      PIC X(121).                              
003206     EJECT                                                                
003207 FD  LISTG                                                                
003208     RECORDING       F                                                    
003209     BLOCK CONTAINS  0.                                                   
003210     SKIP2                                                                
003211 01  LISTGS                      PIC X(121).                              
003212     EJECT                                                                
003213 FD  LISTH                                                                
003214     RECORDING       F                                                    
003215     BLOCK CONTAINS  0.                                                   
003216     SKIP2                                                                
003217 01  LISTHS                      PIC X(121).                              
003218     EJECT                                                                
003220 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W3715400'.            
003410 77   PROGRAM-NAMN           VALUE 'W3715400'                             
003420                                 PIC X(8).                                
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  W-KVRETUR11                 PIC S9(7)   VALUE +0 COMP-3.             
003701 77  W-KVRETUR11-W               PIC S9(7)   VALUE +0 COMP-3.             
003702 77  W-KVRETUR21                 PIC S9(7)   VALUE +0 COMP-3.             
003704 77  W-KVRETUR41                 PIC S9(7)   VALUE +0 COMP-3.             
003705 77  W-KVRETUR41-W               PIC S9(7)   VALUE +0 COMP-3.             
003706 77  W-KVRETUR42                 PIC S9(7)   VALUE +0 COMP-3.             
003707 77  W-KVRETUR43                 PIC S9(7)   VALUE +0 COMP-3.             
003708 77  W-KVRETUR51                 PIC S9(7)   VALUE +0 COMP-3.             
003709 77  W-KVRETUR51-W               PIC S9(7)   VALUE +0 COMP-3.             
003710 77  W-KVRETUR61                 PIC S9(7)   VALUE +0 COMP-3.             
003711 77  W-KVRETUR61-W               PIC S9(7)   VALUE +0 COMP-3.             
003712 77  W-KVRETUR62                 PIC S9(7)   VALUE +0 COMP-3.             
003713 77  W-KVRETUR62-W               PIC S9(7)   VALUE +0 COMP-3.             
003714 77  W-ANTALPOSTER               PIC S9(7)   VALUE +0 COMP-3.             
003715 77  W-ANTALPOSTER-W             PIC S9(7)   VALUE +0 COMP-3.             
003716 77  W-ANTALPOSTER2              PIC S9(7)   VALUE +0 COMP-3.             
003720 77  W-ANTALPOSTER41             PIC S9(7)   VALUE +0 COMP-3.             
003721 77  W-ANTALPOSTER41-W           PIC S9(7)   VALUE +0 COMP-3.             
003722 77  W-ANTALPOSTER42             PIC S9(7)   VALUE +0 COMP-3.             
003723 77  W-ANTALPOSTER43             PIC S9(7)   VALUE +0 COMP-3.             
003730 77  W-ANTALPOSTER5              PIC S9(7)   VALUE +0 COMP-3.             
003740 77  W-ANTALPOSTER5-W            PIC S9(7)   VALUE +0 COMP-3.             
003750 77  W-ANTALPOSTER61             PIC S9(7)   VALUE +0 COMP-3.             
003760 77  W-ANTALPOSTER61-W           PIC S9(7)   VALUE +0 COMP-3.             
003761 77  W-ANTALPOSTER62             PIC S9(7)   VALUE +0 COMP-3.             
003770 77  W-ANTALPOSTER62-W           PIC S9(7)   VALUE +0 COMP-3.             
003800 77  W-ANTALSUMMA                PIC S9(7)V99 VALUE +0 COMP-3.            
003801 77  W-ANTALSUMMA-W              PIC S9(7)V99 VALUE +0 COMP-3.            
003802 77  W-ANTALSUMMA2               PIC S9(7)V99 VALUE +0 COMP-3.            
003804 77  W-ANTALSUMMA41              PIC S9(7)V99 VALUE +0 COMP-3.            
003805 77  W-ANTALSUMMA41-W            PIC S9(7)V99 VALUE +0 COMP-3.            
003806 77  W-ANTALSUMMA42              PIC S9(7)V99 VALUE +0 COMP-3.            
003807 77  W-ANTALSUMMA43              PIC S9(7)V99 VALUE +0 COMP-3.            
003808 77  W-ANTALSUMMA5               PIC S9(7)V99 VALUE +0 COMP-3.            
003809 77  W-ANTALSUMMA5-W             PIC S9(7)V99 VALUE +0 COMP-3.            
003810 77  W-ANTALSUMMA61              PIC S9(7)V99 VALUE +0 COMP-3.            
003811 77  W-ANTALSUMMA61-W            PIC S9(7)V99 VALUE +0 COMP-3.            
003812 77  W-ANTALSUMMA62              PIC S9(7)V99 VALUE +0 COMP-3.            
003813 77  W-ANTALSUMMA62-W            PIC S9(7)V99 VALUE +0 COMP-3.            
003814 77  W-KVARBDAG                  PIC S9(7)   VALUE +0 COMP-3.             
003815 77  W-KVARBDAG-W                PIC S9(7)   VALUE +0 COMP-3.             
003816 77  W-KVARBDAG2                 PIC S9(7)   VALUE +0 COMP-3.             
003817 77  W-KVARBDAG2-W               PIC S9(7)   VALUE +0 COMP-3.             
003818 77  W-KVARBDAG41                PIC S9(7)   VALUE +0 COMP-3.             
003819 77  W-KVARBDAG41-W              PIC S9(7)   VALUE +0 COMP-3.             
003820 77  W-KVARBDAG42                PIC S9(7)   VALUE +0 COMP-3.             
003821 77  W-KVARBDAG43                PIC S9(7)   VALUE +0 COMP-3.             
003822 77  W-KVARBDAG5                 PIC S9(7)   VALUE +0 COMP-3.             
003823 77  W-KVARBDAG5-W               PIC S9(7)   VALUE +0 COMP-3.             
003824 77  W-KVARBDAG61                PIC S9(7)   VALUE +0 COMP-3.             
003825 77  W-KVARBDAG61-W              PIC S9(7)   VALUE +0 COMP-3.             
003826 77  W-KVARBDAG62                PIC S9(7)   VALUE +0 COMP-3.             
003827 77  W-KVARBDAG62-W              PIC S9(7)   VALUE +0 COMP-3.             
003828                                                                          
003830                                                                          
003831 01  SPAR-FLBYTGAR-41            PIC X       VALUE SPACE.                 
003832 01  SPAR-FLBYTGAR-42            PIC X       VALUE SPACE.                 
003833 01  SPAR-FLBYTGAR-43            PIC X       VALUE SPACE.                 
003834 01  SPAR-FLBYTGAR-51            PIC X       VALUE SPACE.                 
003835 01  SPAR-FLBYTGAR-61            PIC X       VALUE SPACE.                 
003836 01  SPAR-FLBYTGAR-62            PIC X       VALUE SPACE.                 
003837                                                                          
003838 77  W3714D-EOF-SW               PIC X       VALUE 'N'.                   
003840     88  END-OF-W3714D                       VALUE 'J'.                   
003900     EJECT                                                                
003960     EJECT                                                                
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004510                                                                          
004520 01  WS-DATUM.                                                            
004530     03  DATUM-SEKEL             PIC X(2).                                
004532     03  DATUM-AAR               PIC X(2).                                
004533     03  FILLER                  PIC X(1)    VALUE '-'.                   
004540     03  DATUM-MAANAD            PIC X(2).                                
004541     03  FILLER                  PIC X(1)    VALUE '-'.                   
004550     03  DATUM-DAG               PIC X(2).                                
004551                                                                          
004552                                                                          
004553 01  WS-DATUM-GB.                                                         
004554     03  DATUM-GB-DAG            PIC X(2).                                
004557     03  FILLER                  PIC X(1)    VALUE '-'.                   
004558     03  DATUM-GB-MAANAD         PIC X(2).                                
004559     03  FILLER                  PIC X(1)    VALUE '-'.                   
004560     03  DATUM-GB-SEKEL          PIC X(2).                                
004561     03  DATUM-GB-AAR            PIC X(2).                                
004562                                                                          
004563                                                                          
004564                                                                          
004565 01  WS-DATUM-US.                                                         
004566     03  DATUM-US-MAANAD         PIC X(2).                                
004568     03  FILLER                  PIC X(1)    VALUE '-'.                   
004569     03  DATUM-US-DAG            PIC X(2).                                
004570     03  FILLER                  PIC X(1)    VALUE '-'.                   
004571     03  DATUM-US-SEKEL          PIC X(2).                                
004572     03  DATUM-US-AAR            PIC X(2).                                
004573                                                                          
004580     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004920     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
005000     SKIP2                                                                
005010*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
005020 01  FILLER                      PIC X(16)   VALUE 'DATKORT'.             
005030 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
005040     SKIP2                                                                
005050*01  -COPY WDATKORT                                                       
005060*    --- VALID IDDC CODES                                                 
005070*                                                                         
005080*01 -COPY WWDC99                                                          
005090*                                                                         
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006110*01  -COPY WDATAREA                                                       
006201     EJECT                                                                
006202 01  IN-AREA-START               PIC X(24)   VALUE                        
006203                                 'IN-AREA-START  '.                       
006204     SKIP2                                                                
006205                                                                          
006206*01  AREA -COPY W3714D     -PRE IN-                                       
006207     EJECT                                                                
006208 01  W001-AREA-START             PIC X(24)   VALUE                        
006209                                 'LIST-AREA-START  '.                     
006210     SKIP2                                                                
006223 01  UT-RAD                      PIC X(121)  VALUE SPACE.                 
006224 01  UT-RAD-91                   PIC X(121)  VALUE SPACE.                 
006225 01  UT-RAD-51                   PIC X(121)  VALUE SPACE.                 
006226 01  UT-RAD-41                   PIC X(121)  VALUE SPACE.                 
006227 01  UT-RAD-42                   PIC X(121)  VALUE SPACE.                 
006228 01  UT-RAD-43                   PIC X(121)  VALUE SPACE.                 
006229 01  UT-RAD-61                   PIC X(121)  VALUE SPACE.                 
006230 01  UT-RAD-62                   PIC X(121)  VALUE SPACE.                 
006231*                                                                         
006232     EJECT                                                                
006233 01  W001R1-RUBRIK-S.                                                     
006234*                                                                         
006235     03  W001R1-STYR           PIC X(1)   VALUE '1'.                      
006236     03  FILLER                PIC X(2)   VALUE SPACE.                    
006237     03  FILLER                PIC X(15)  VALUE 'W37154-011'.             
006238     03  FILLER                PIC X(34)                                  
006239                  VALUE 'SNITTLEDTID FÖR BYTESRETURER FRÅN '.             
006240     03  FILLER                PIC X(39)                                  
006241                  VALUE 'STATUS 3 TILL STATUS 4 DAGLIG LISTA'.            
006242     03  FILLER                PIC X(7)   VALUE 'DATUM'.                  
006243     03  W001R1-DAGENS-DATUM   PIC X(10)  VALUE SPACE.                    
006244     03  FILLER                PIC X(5)   VALUE SPACE.                    
006245     03  FILLER                PIC X(7)  VALUE 'SIDA  1'.                 
006246     EJECT                                                                
006247 01  W001R1-RUBRIK-S21.                                                   
006248*                                                                         
006249*    03  W001R1-STYR-91        PIC X(1)   VALUE ';'.                      
006250     03  FILLER                PIC X(2)   VALUE SPACE.                    
006251     03  FILLER                PIC X(10)  VALUE 'W37154-091'.             
006252     03  FILLER                PIC X(1)   VALUE ';'.                      
006253     03  FILLER                PIC X(34)                                  
006254                  VALUE 'AVERAGE LEADTIME ON EXCH.RETURNS.;'.             
006255     03  FILLER                PIC X(29)                                  
006256                  VALUE 'FROM STATUS 3 TO 4 DAILY LIST'.                  
006257     03  FILLER                PIC X(1)   VALUE ';'.                      
006258     03  FILLER                PIC X(5)   VALUE 'DATE '.                  
006259     03  W001R1-DAGENS-DATUM21 PIC X(10).                                 
006261     03  FILLER                PIC X(1)   VALUE ';'.                      
006262*    03  FILLER                PIC X(7)  VALUE 'PAGE  1'.                 
006263     EJECT                                                                
006264                                                                          
006265 01  W001R1-RUBRIK-S51.                                                   
006266*                                                                         
006267     03  W001R1-STYR-51        PIC X(1)   VALUE '1'.                      
006268     03  FILLER                PIC X(2)   VALUE SPACE.                    
006269     03  FILLER                PIC X(10)  VALUE 'W37154-051'.             
006270     03  FILLER                PIC X(2)   VALUE SPACE.                    
006271     03  FILLER                PIC X(34)                                  
006272                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
006273     03  FILLER                PIC X(40)                                  
006274                  VALUE 'E A CORE DAILY LIST          '.                  
006275     03  FILLER                PIC X(3)   VALUE SPACE.                    
006276     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
006277     03  W001R1-DAGENS-DATUM51 PIC X(10).                                 
006278     03  FILLER                PIC X(5)   VALUE SPACE.                    
006279     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
006280                                                                          
006281     EJECT                                                                
006282****  USA RUBRIKER ****                                                   
006283                                                                          
006284 01  W001R1-RUBRIK-S41.                                                   
006285*                                                                         
006286     03  W001R1-STYR-41        PIC X(1)   VALUE '1'.                      
006287     03  FILLER                PIC X(2)   VALUE SPACE.                    
006288     03  FILLER                PIC X(10)  VALUE 'W37154-041'.             
006289     03  FILLER                PIC X(2)   VALUE SPACE.                    
006290     03  FILLER                PIC X(34)                                  
006291                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
006292     03  FILLER                PIC X(40)                                  
006293                  VALUE 'E A CORE DAILY LIST          '.                  
006294     03  FILLER                PIC X(3)   VALUE SPACE.                    
006295     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
006296     03  W001R1-DAGENS-DATUM41 PIC X(10).                                 
006297     03  FILLER                PIC X(5)   VALUE SPACE.                    
006298     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
006299                                                                          
006300                                                                          
006301 01  W001R1-RUBRIK-S42.                                                   
006302*                                                                         
006303     03  W001R1-STYR-42        PIC X(1)   VALUE '1'.                      
006304     03  FILLER                PIC X(2)   VALUE SPACE.                    
006305     03  FILLER                PIC X(15)  VALUE 'W37154-042'.             
006306     03  FILLER                PIC X(34)                                  
006307                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
006308     03  FILLER                PIC X(40)                                  
006309                  VALUE 'E A CORE DAILY LIST          '.                  
006310     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
006311     03  W001R1-DAGENS-DATUM42 PIC X(10).                                 
006312     03  FILLER                PIC X(5)   VALUE SPACE.                    
006313     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
006314                                                                          
006315                                                                          
006316 01  W001R1-RUBRIK-S43.                                                   
006317*                                                                         
006318     03  W001R1-STYR-43        PIC X(1)   VALUE '1'.                      
006319     03  FILLER                PIC X(2)   VALUE SPACE.                    
006320     03  FILLER                PIC X(15)  VALUE 'W37154-043'.             
006321     03  FILLER                PIC X(34)                                  
006322                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
006323     03  FILLER                PIC X(40)                                  
006324                  VALUE 'E A CORE DAILY LIST          '.                  
006325     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
006326     03  W001R1-DAGENS-DATUM43 PIC X(10).                                 
006327     03  FILLER                PIC X(5)   VALUE SPACE.                    
006328     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
006329                                                                          
006330                                                                          
006331 01  W001R1-RUBRIK-S61.                                                   
006332*                                                                         
006333     03  W001R1-STYR-61        PIC X(1)   VALUE '1'.                      
006340     03  FILLER                PIC X(2)   VALUE SPACE.                    
006350     03  FILLER                PIC X(15)  VALUE 'W37154-061'.             
006360     03  FILLER                PIC X(34)                                  
006370                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
006380     03  FILLER                PIC X(40)                                  
006381                  VALUE 'E A CORE DAILY LIST          '.                  
006382     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
006383     03  W001R1-DAGENS-DATUM61 PIC X(10).                                 
006384     03  FILLER                PIC X(5)   VALUE SPACE.                    
006385     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
006386                                                                          
006387                                                                          
006388 01  W001R1-RUBRIK-S62.                                                   
006389*                                                                         
006390     03  W001R1-STYR-62        PIC X(1)   VALUE '1'.                      
006391     03  FILLER                PIC X(2)   VALUE SPACE.                    
006392     03  FILLER                PIC X(15)  VALUE 'W37154-062'.             
006393     03  FILLER                PIC X(34)                                  
006394                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
006395     03  FILLER                PIC X(40)                                  
006396                  VALUE 'E A CORE DAILY LIST          '.                  
006397     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
006398     03  W001R1-DAGENS-DATUM62 PIC X(10).                                 
006399     03  FILLER                PIC X(5)   VALUE SPACE.                    
006400     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
006401                                                                          
006402 01  W001R2-DELRUBRIK-1.                                                  
006403*                                                                         
006404     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
006405     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006406     03  FILLER                PIC X(7)   VALUE 'DC '.                    
006407     03  FILLER                PIC X(6)   VALUE '     '.                  
006408     03  FILLER                PIC X(17)                                  
006409                         VALUE 'SNITT ARBETSTIDEN'.                       
006410     03  FILLER                PIC X(2)   VALUE SPACE.                    
006411     03  FILLER                PIC X(17)                                  
006412                         VALUE 'SNITT ARBETSTIDEN'.                       
006413     03  FILLER                PIC X(17)                                  
006414                         VALUE '  ANTAL ARTIKLAR '.                       
006415     03  FILLER                PIC X(11) VALUE SPACE.                     
006416                                                                          
006417     EJECT                                                                
006418*                                                                         
006419 01  W001R2-DELRUBRIK-3.                                                  
006420     03  FILLER                PIC X(1)   VALUE '0'.                      
006421     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006422     03  FILLER                PIC X(7)   VALUE '   '.                    
006423     03  FILLER                PIC X(6)   VALUE '     '.                  
006424     03  FILLER                PIC X(23)                                  
006425               VALUE '       EJ GARANTI      '.                           
006426     03  FILLER                PIC X(14)                                  
006427               VALUE '      GARANTI '.                                    
006428     03  FILLER                PIC X(2)   VALUE SPACE.                    
006429     03  FILLER                PIC X(4)   VALUE SPACE.                    
006430     03  FILLER                PIC X(21)  VALUE SPACE.                    
006431                                                                          
006432                                                                          
006433 01  W001R2-DELRUBRIK-1-91.                                               
006434*                                                                         
006435*    03  W001R2-STYR-91        PIC X(1)   VALUE ';'.                      
006436*    03  FILLER                PIC X(4)   VALUE  SPACE.                   
006437     03  FILLER                PIC X(3)   VALUE 'DC;'.                    
006439     03  FILLER                PIC X(32)                                  
006440                VALUE 'WORKTIME AVERAGE; TOTAL WORKTIME'.                 
006441     03  FILLER                PIC X(14)                                  
006442                VALUE ';TOTAL REPORTS'.                                   
006443     03  FILLER                PIC X(1)   VALUE ';'.                      
006444                                                                          
006445     EJECT                                                                
006450                                                                          
006459 01  W001R2-DELRUBRIK-3-51.                                               
006460     03  FILLER                PIC X(1)   VALUE '0'.                      
006461     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006462     03  FILLER                PIC X(7)   VALUE  SPACE.                   
006463     03  FILLER                PIC X(3)   VALUE  SPACE.                   
006465     03  FILLER                PIC X(31)                                  
006466               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
006472     03  FILLER                PIC X(31)                                  
006473               VALUE '           WARRANTY  WARRANTY  '.                   
006474     03  FILLER                PIC X(3)   VALUE SPACE.                    
006475                                                                          
006476     EJECT                                                                
006477                                                                          
006478                                                                          
006479 01  W001R2-DELRUBRIK-3-61.                                               
006480     03  FILLER                PIC X(1)   VALUE '0'.                      
006481     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006482     03  FILLER                PIC X(7)   VALUE  SPACE.                   
006483     03  FILLER                PIC X(3)   VALUE  SPACE.                   
006484     03  FILLER                PIC X(31)                                  
006485               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
006486     03  FILLER                PIC X(31)                                  
006487               VALUE '           WARRANTY  WARRANTY  '.                   
006488     03  FILLER                PIC X(3)   VALUE SPACE.                    
006489                                                                          
006490     EJECT                                                                
006491                                                                          
006492 01  W001R2-DELRUBRIK-3-62.                                               
006493     03  FILLER                PIC X(1)   VALUE '0'.                      
006494     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006495     03  FILLER                PIC X(7)   VALUE  SPACE.                   
006496     03  FILLER                PIC X(3)   VALUE  SPACE.                   
006497     03  FILLER                PIC X(31)                                  
006498               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
006499     03  FILLER                PIC X(31)                                  
006500               VALUE '           WARRANTY  WARRANTY  '.                   
006501     03  FILLER                PIC X(3)   VALUE SPACE.                    
006502                                                                          
006503     EJECT                                                                
006504                                                                          
006505 01  W001R2-DELRUBRIK-1-51.                                               
006506*                                                                         
006507     03  W001R2-STYR-51        PIC X(1)   VALUE '0'.                      
006508     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006509     03  FILLER                PIC X(7)   VALUE 'DC '.                    
006510     03  FILLER                PIC X(3)   VALUE '   '.                    
006511     03  FILLER                PIC X(26)                                  
006512               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006513     03  FILLER                PIC X(8)   VALUE SPACE.                    
006514     03  FILLER                PIC X(26)                                  
006515               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006516     03  FILLER                PIC X(111)  VALUE SPACE.                   
006517                                                                          
006518                                                                          
006519 01  W001R2-DELRUBRIK-1-61.                                               
006520*                                                                         
006521     03  W001R2-STYR-61        PIC X(1)   VALUE '0'.                      
006522     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006523     03  FILLER                PIC X(7)   VALUE 'DC '.                    
006524     03  FILLER                PIC X(3)   VALUE '   '.                    
006525     03  FILLER                PIC X(26)                                  
006526               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006527     03  FILLER                PIC X(8)   VALUE SPACE.                    
006528     03  FILLER                PIC X(26)                                  
006529               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006530     03  FILLER                PIC X(111)  VALUE SPACE.                   
006531                                                                          
006532     EJECT                                                                
006533                                                                          
006534 01  W001R2-DELRUBRIK-1-62.                                               
006535*                                                                         
006536     03  W001R2-STYR-62        PIC X(1)   VALUE '0'.                      
006537     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006538     03  FILLER                PIC X(7)   VALUE 'DC '.                    
006539     03  FILLER                PIC X(3)   VALUE '   '.                    
006540     03  FILLER                PIC X(26)                                  
006541               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006542     03  FILLER                PIC X(8)   VALUE SPACE.                    
006543     03  FILLER                PIC X(26)                                  
006544               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006545     03  FILLER                PIC X(111)  VALUE SPACE.                   
006546                                                                          
006547     EJECT                                                                
006548*** USA RUBRIKER ****                                                     
006549*                                                                         
006550                                                                          
006551 01  W001R2-DELRUBRIK-3-41.                                               
006552     03  FILLER                PIC X(1)   VALUE '0'.                      
006553     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006554     03  FILLER                PIC X(7)   VALUE  SPACE.                   
006555     03  FILLER                PIC X(3)   VALUE  SPACE.                   
006556     03  FILLER                PIC X(31)                                  
006557               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
006558     03  FILLER                PIC X(31)                                  
006559               VALUE '           WARRANTY  WARRANTY  '.                   
006560     03  FILLER                PIC X(3)   VALUE SPACE.                    
006561                                                                          
006562     EJECT                                                                
006563                                                                          
006564                                                                          
006565*** USA RUBRIKER ****                                                     
006566 01  W001R2-DELRUBRIK-1-41.                                               
006567*                                                                         
006568     03  W001R2-STYR-41        PIC X(1)   VALUE '0'.                      
006569     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006570     03  FILLER                PIC X(7)   VALUE 'DC '.                    
006571     03  FILLER                PIC X(3)   VALUE '   '.                    
006572     03  FILLER                PIC X(26)                                  
006573               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006574     03  FILLER                PIC X(8)   VALUE SPACE.                    
006575     03  FILLER                PIC X(26)                                  
006576               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
006577     03  FILLER                PIC X(111)  VALUE SPACE.                   
006578                                                                          
006579                                                                          
006580 01  W001R2-DELRUBRIK-1-42.                                               
006581*                                                                         
006582     03  W001R2-STYR-42        PIC X(1)   VALUE '0'.                      
006583     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006584     03  FILLER                PIC X(7)   VALUE 'DC '.                    
006585     03  FILLER                PIC X(6)   VALUE '     '.                  
006586     03  FILLER                PIC X(32)                                  
006587               VALUE 'WORKTIME AVERAGE        QUANTITY'.                  
006588     03  FILLER                PIC X(7)  VALUE SPACE.                     
006589     03  FILLER                PIC X(2)   VALUE SPACE.                    
006590     03  FILLER                PIC X(8)   VALUE SPACE.                    
006591     03  FILLER                PIC X(4)   VALUE SPACE.                    
006592     03  FILLER                PIC X(13)  VALUE SPACE.                    
006593                                                                          
006594                                                                          
006595 01  W001R2-DELRUBRIK-1-43.                                               
006596*                                                                         
006597     03  W001R2-STYR-43        PIC X(1)   VALUE '0'.                      
006598     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006599     03  FILLER                PIC X(7)   VALUE 'DC '.                    
006600     03  FILLER                PIC X(6)   VALUE '     '.                  
006601     03  FILLER                PIC X(32)                                  
006602               VALUE 'WORKTIME AVERAGE        QUANTITY'.                  
006603     03  FILLER                PIC X(3)  VALUE SPACE.                     
006604     03  FILLER                PIC X(2)   VALUE SPACE.                    
006605     03  FILLER                PIC X(8)   VALUE SPACE.                    
006606     03  FILLER                PIC X(4)   VALUE SPACE.                    
006607     03  FILLER                PIC X(13)  VALUE SPACE.                    
006608                                                                          
006609     EJECT                                                                
006610 01  W001R2-DELRUBRIK-2.                                                  
006611*                                                                         
006612     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
006613     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006614     03  FILLER                PIC X(2)   VALUE '11'.                     
006615     03  FILLER                PIC X(18)  VALUE SPACE.                    
006616     03  W001R2-ANTAL          PIC Z(7).99.                               
006617     03  FILLER                PIC X(9)  VALUE SPACE.                     
006618     03  W001R2-ANTAL-W        PIC Z(7).99.                               
006619     03  FILLER                PIC X(9)  VALUE SPACE.                     
006620     03  W001R2-KVRETUR-11     PIC Z(6)9.                                 
006621     03  FILLER                PIC X(9)  VALUE SPACE.                     
006622                                                                          
006623                                                                          
006624     EJECT                                                                
006625*****  BORN RUBRIKER *****                                                
006626 01  W001R2-DELRUBRIK-2-91.                                               
006627*                                                                         
006628*    03  W001R2-STYR-91        PIC X(1)   VALUE ';'.                      
006629*    03  FILLER                PIC X(4)   VALUE  SPACE.                   
006630     03  FILLER                PIC X(3)   VALUE '91;'.                    
006631*    03  FILLER                PIC X(17)  VALUE SPACE.                    
006632     03  W001R2-ANTAL-91       PIC Z(7).99.                               
006633     03  FILLER                PIC X(01) VALUE ';'.                       
006634     03  W001R2-WORKDAY-91     PIC Z(6)9.                                 
006635     03  FILLER                PIC X(1)  VALUE ';'.                       
006636     03  W001R2-TOTREPT-91     PIC Z(6)9.                                 
006637                                                                          
006638     EJECT                                                                
006639*****  CANADA RUBRIKER *****                                              
006640 01  W001R2-DELRUBRIK-2-51.                                               
006641*                                                                         
006642     03  W001R2-STYR-51        PIC X(1)   VALUE '0'.                      
006643     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006644     03  FILLER                PIC X(2)   VALUE '51'.                     
006645     03  FILLER                PIC X(14)  VALUE SPACE.                    
006646     03  W001R2-ANTAL-51       PIC Z(7).99.                               
006647     03  FILLER                PIC X(8)   VALUE SPACE.                    
006648     03  W001R2-KVRETUR-51     PIC Z(6)9.                                 
006649     03  FILLER                PIC X(9)   VALUE SPACE.                    
006650     03  W001R2-ANTAL-51-W     PIC Z(7).99.                               
006651     03  FILLER                PIC X(3)   VALUE SPACE.                    
006652     03  W001R2-KVRETUR-51-W   PIC Z(6)9.                                 
006653     03  FILLER                PIC X(7)  VALUE SPACE.                     
006654                                                                          
006655     EJECT                                                                
006656*****  JAPAN RUBRIKER *****                                               
006657 01  W001R2-DELRUBRIK-2-61.                                               
006658*                                                                         
006659     03  W001R2-STYR-61        PIC X(1)   VALUE '0'.                      
006660     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006661     03  FILLER                PIC X(2)   VALUE '61'.                     
006662     03  FILLER                PIC X(14)  VALUE SPACE.                    
006663     03  W001R2-ANTAL-61       PIC Z(7).99.                               
006664     03  FILLER                PIC X(8)   VALUE SPACE.                    
006665     03  W001R2-KVRETUR-61     PIC Z(6)9.                                 
006666     03  FILLER                PIC X(9)   VALUE SPACE.                    
006667     03  W001R2-ANTAL-61-W     PIC Z(7).99.                               
006668     03  FILLER                PIC X(3)   VALUE SPACE.                    
006669     03  W001R2-KVRETUR-61-W   PIC Z(6)9.                                 
006670     03  FILLER                PIC X(7)  VALUE SPACE.                     
006671                                                                          
006672     EJECT                                                                
006673*****  AUSTRALIEN RUBRIKER ****                                           
006674 01  W001R2-DELRUBRIK-2-62.                                               
006675*                                                                         
006676     03  W001R2-STYR-62        PIC X(1)   VALUE '0'.                      
006677     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006678     03  FILLER                PIC X(2)   VALUE '62'.                     
006679     03  FILLER                PIC X(14)  VALUE SPACE.                    
006680     03  W001R2-ANTAL-62       PIC Z(7).99.                               
006681     03  FILLER                PIC X(8)   VALUE SPACE.                    
006682     03  W001R2-KVRETUR-62     PIC Z(6)9.                                 
006683     03  FILLER                PIC X(9)   VALUE SPACE.                    
006684     03  W001R2-ANTAL-62-W     PIC Z(7).99.                               
006685     03  FILLER                PIC X(3)   VALUE SPACE.                    
006686     03  W001R2-KVRETUR-62-W   PIC Z(6)9.                                 
006687     03  FILLER                PIC X(7)  VALUE SPACE.                     
006688                                                                          
006689     EJECT                                                                
006690*****  USA RUBRIKER *****                                                 
006691 01  W001R2-DELRUBRIK-2-41.                                               
006692*                                                                         
006693     03  W001R2-STYR-41        PIC X(1)   VALUE '0'.                      
006694     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006695     03  FILLER                PIC X(2)   VALUE '41'.                     
006696     03  FILLER                PIC X(14)  VALUE SPACE.                    
006697     03  W001R2-ANTAL-41       PIC Z(7).99.                               
006698     03  FILLER                PIC X(8)   VALUE SPACE.                    
006699     03  W001R2-KVRETUR-41     PIC Z(6)9.                                 
006700     03  FILLER                PIC X(9)   VALUE SPACE.                    
006701     03  W001R2-ANTAL-41-W     PIC Z(7).99.                               
006702     03  FILLER                PIC X(3)   VALUE SPACE.                    
006703     03  W001R2-KVRETUR-41-W   PIC Z(6)9.                                 
006704     03  FILLER                PIC X(7)  VALUE SPACE.                     
006705                                                                          
006706 01  W001R2-DELRUBRIK-2-42.                                               
006707*                                                                         
006708     03  W001R2-STYR-42        PIC X(1)   VALUE '0'.                      
006709     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006710     03  FILLER                PIC X(2)   VALUE '42'.                     
006711     03  FILLER                PIC X(17)  VALUE SPACE.                    
006712     03  W001R2-ANTAL-42       PIC Z(7).99.                               
006713     03  FILLER                PIC X(9)   VALUE SPACE.                    
006714     03  W001R2-KVRETUR-42     PIC Z(6)9.                                 
006715     03  FILLER                PIC X(8)   VALUE SPACE.                    
006716     03  FILLER                PIC X(3)   VALUE SPACE.                    
006717     03  FILLER                PIC X(13)  VALUE SPACE.                    
006718                                                                          
006719 01  W001R2-DELRUBRIK-2-43.                                               
006720*                                                                         
006721     03  W001R2-STYR-43        PIC X(1)   VALUE '0'.                      
006722     03  FILLER                PIC X(4)   VALUE  SPACE.                   
006723     03  FILLER                PIC X(2)   VALUE '43'.                     
006724     03  FILLER                PIC X(17)  VALUE SPACE.                    
006725     03  W001R2-ANTAL-43       PIC Z(7).99.                               
006726     03  FILLER                PIC X(9)   VALUE SPACE.                    
006727     03  W001R2-KVRETUR-43     PIC Z(6)9.                                 
006728     03  FILLER                PIC X(2)   VALUE SPACE.                    
006729     03  FILLER                PIC X(8)   VALUE SPACE.                    
006730     03  FILLER                PIC X(3)   VALUE SPACE.                    
006731     03  FILLER                PIC X(13)  VALUE SPACE.                    
006732     EJECT                                                                
006733                                                                          
006734 PROCEDURE DIVISION.                                                      
006735 MAIN SECTION.                                                            
006740     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-LAES-W3714D                                              
007100     PERFORM UNTIL END-OF-W3714D                                          
007200       PERFORM B-BEARBETA                                                 
007810       PERFORM S01-LAES-W3714D                                            
007900     END-PERFORM                                                          
008000                                                                          
008010     PERFORM C-BERAKNA                                                    
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008902*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
008903                                                                          
008904     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
008905                                                                          
008906     MOVE   D-AAR            TO  DAGENS-DATUM-AAR                         
008907     MOVE   D-MAANAD         TO  DAGENS-DATUM-MAANAD                      
008908     MOVE   D-DAG            TO  DAGENS-DATUM-DAG                         
008909                                                                          
008910     IF DAGENS-DATUM-AAR > 50                                             
008911        MOVE 19              TO DATUM-SEKEL                               
008912                                DATUM-GB-SEKEL                            
008913                                DATUM-US-SEKEL                            
008914     ELSE                                                                 
008915        MOVE 20              TO DATUM-SEKEL                               
008916                                DATUM-GB-SEKEL                            
008917                                DATUM-US-SEKEL                            
008920     END-IF                                                               
008921                                                                          
008922     MOVE DAGENS-DATUM-AAR    TO DATUM-AAR                                
008923                                 DATUM-GB-AAR                             
008924                                 DATUM-US-AAR                             
008925     MOVE DAGENS-DATUM-MAANAD TO DATUM-MAANAD                             
008926                                 DATUM-GB-MAANAD                          
008927                                 DATUM-US-MAANAD                          
008928     MOVE DAGENS-DATUM-DAG    TO DATUM-DAG                                
008929                                 DATUM-GB-DAG                             
008930                                 DATUM-US-DAG                             
008931                                                                          
008932     MOVE WS-DATUM            TO W001R1-DAGENS-DATUM                      
008933                                 W001R1-DAGENS-DATUM61                    
008934     MOVE WS-DATUM-GB         TO W001R1-DAGENS-DATUM21                    
008935                                 W001R1-DAGENS-DATUM62                    
008936     MOVE WS-DATUM-US         TO W001R1-DAGENS-DATUM51                    
008937                                 W001R1-DAGENS-DATUM41                    
008938                                 W001R1-DAGENS-DATUM42                    
008940                                 W001R1-DAGENS-DATUM43                    
008942                                                                          
008943                                                                          
008950     OPEN INPUT  W3714D                                                   
009001                                                                          
009010     OPEN OUTPUT LISTA                                                    
009011                 LISTB                                                    
009012                 LISTC                                                    
009013                 LISTD                                                    
009014                 LISTE                                                    
009015                 LISTF                                                    
009016                 LISTG                                                    
009017                 LISTH                                                    
009018                                                                          
009019***** RUBRIKRAD 1 ******                                                  
009020                                                                          
009021     MOVE W001R1-RUBRIK-S     TO UT-RAD                                   
009022     PERFORM S02-SKRIV-UT-RAD                                             
009023                                                                          
009024     MOVE W001R1-RUBRIK-S21   TO UT-RAD-91                                
009025     PERFORM S03-SKRIV-UT-RAD-91                                          
009026                                                                          
009027     MOVE W001R1-RUBRIK-S51   TO UT-RAD-51                                
009028     PERFORM S04-SKRIV-UT-RAD-51                                          
009029                                                                          
009030     MOVE W001R1-RUBRIK-S41   TO UT-RAD-41                                
009031     PERFORM S05-SKRIV-UT-RAD-41                                          
009032                                                                          
009033     MOVE W001R1-RUBRIK-S42   TO UT-RAD-42                                
009034     PERFORM S06-SKRIV-UT-RAD-42                                          
009035                                                                          
009036     MOVE W001R1-RUBRIK-S43   TO UT-RAD-43                                
009037     PERFORM S07-SKRIV-UT-RAD-43                                          
009039                                                                          
009040     MOVE W001R1-RUBRIK-S61   TO UT-RAD-61                                
009041     PERFORM S08-SKRIV-UT-RAD-61                                          
009042                                                                          
009044     MOVE W001R1-RUBRIK-S62   TO UT-RAD-62                                
009045     PERFORM S09-SKRIV-UT-RAD-62                                          
009046                                                                          
009047***** RUBRIKRAD 2 ******                                                  
009048                                                                          
009049     MOVE W001R2-DELRUBRIK-1  TO UT-RAD                                   
009050     PERFORM S02-SKRIV-UT-RAD                                             
009051                                                                          
009052     MOVE W001R2-DELRUBRIK-3  TO UT-RAD                                   
009053     PERFORM S02-SKRIV-UT-RAD                                             
009054                                                                          
009055     MOVE W001R2-DELRUBRIK-1-91  TO UT-RAD-91                             
009056     PERFORM S03-SKRIV-UT-RAD-91                                          
009057                                                                          
009058     MOVE W001R2-DELRUBRIK-1-51  TO UT-RAD-51                             
009059     PERFORM S04-SKRIV-UT-RAD-51                                          
009060                                                                          
009061     MOVE W001R2-DELRUBRIK-3-51  TO UT-RAD-51                             
009062     PERFORM S04-SKRIV-UT-RAD-51                                          
009063                                                                          
009070     MOVE W001R2-DELRUBRIK-1-41  TO UT-RAD-41                             
009080     PERFORM S05-SKRIV-UT-RAD-41                                          
009081                                                                          
009082     MOVE W001R2-DELRUBRIK-3-41  TO UT-RAD-41                             
009083     PERFORM S05-SKRIV-UT-RAD-41                                          
009084                                                                          
009085     MOVE W001R2-DELRUBRIK-1-42  TO UT-RAD-42                             
009086     PERFORM S06-SKRIV-UT-RAD-42                                          
009087                                                                          
009088     MOVE W001R2-DELRUBRIK-1-43  TO UT-RAD-43                             
009089     PERFORM S07-SKRIV-UT-RAD-43                                          
009090                                                                          
009092     MOVE W001R2-DELRUBRIK-1-61  TO UT-RAD-61                             
009093     PERFORM S08-SKRIV-UT-RAD-61                                          
009094                                                                          
009095     MOVE W001R2-DELRUBRIK-3-61  TO UT-RAD-61                             
009096     PERFORM S08-SKRIV-UT-RAD-61                                          
009097                                                                          
009099     MOVE W001R2-DELRUBRIK-1-62  TO UT-RAD-62                             
009100     PERFORM S09-SKRIV-UT-RAD-62                                          
009101                                                                          
009102     MOVE W001R2-DELRUBRIK-3-62  TO UT-RAD-62                             
009103     PERFORM S09-SKRIV-UT-RAD-62                                          
009104                                                                          
009110     SKIP2                                                                
009200     ACCEPT DAGENS-DATUM  FROM DATE                                       
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009510 B-BEARBETA SECTION.                                                      
009512                                                                          
009551     MOVE IN-IDDC            TO WS-IDDC                                   
009557     EVALUATE TRUE                                                        
009558         WHEN CDC-SE                                                      
009559           IF IN-FLBYTGAR = 'J'                                           
009560             ADD +1            TO W-ANTALPOSTER-W                         
009561             ADD IN-KVARBDAG   TO W-KVARBDAG-W                            
009562             ADD IN-KVRETUR    TO W-KVRETUR11                             
009563           ELSE                                                           
009564             ADD +1            TO W-ANTALPOSTER                           
009565             ADD IN-KVARBDAG   TO W-KVARBDAG                              
009566             ADD IN-KVRETUR    TO W-KVRETUR11                             
009567           END-IF                                                         
009568         WHEN SDC-NL-ET                                                   
009574             ADD +1            TO W-ANTALPOSTER2                          
009575             ADD IN-KVARBDAG   TO W-KVARBDAG2                             
009576             ADD IN-KVRETUR    TO W-KVRETUR21                             
009578         WHEN NDC-US-RU                                                   
009579           IF IN-FLBYTGAR = 'J'                                           
009580              ADD +1            TO W-ANTALPOSTER41-W                      
009581              ADD IN-KVARBDAG   TO W-KVARBDAG41-W                         
009582              ADD IN-KVRETUR    TO W-KVRETUR41-W                          
009583           ELSE                                                           
009584              ADD +1            TO W-ANTALPOSTER41                        
009585              ADD IN-KVARBDAG   TO W-KVARBDAG41                           
009586              ADD IN-KVRETUR    TO W-KVRETUR41                            
009587           END-IF                                                         
009592         WHEN NDC-US-LA                                                   
009593              ADD +1            TO W-ANTALPOSTER43                        
009594              ADD IN-KVARBDAG   TO W-KVARBDAG43                           
009595              ADD IN-KVRETUR    TO W-KVRETUR43                            
009596         WHEN NDC-CA                                                      
009597           IF IN-FLBYTGAR = 'J'                                           
009598              ADD +1            TO W-ANTALPOSTER5-W                       
009599              ADD IN-KVARBDAG   TO W-KVARBDAG5-W                          
009600              ADD IN-KVRETUR    TO W-KVRETUR51-W                          
009601           ELSE                                                           
009602              ADD +1            TO W-ANTALPOSTER5                         
009603              ADD IN-KVARBDAG   TO W-KVARBDAG5                            
009604              ADD IN-KVRETUR    TO W-KVRETUR51                            
009605           END-IF                                                         
009606         WHEN NDC-JP                                                      
009607           IF IN-FLBYTGAR = 'J'                                           
009608              ADD +1            TO W-ANTALPOSTER61-W                      
009609              ADD IN-KVARBDAG   TO W-KVARBDAG61-W                         
009610              ADD IN-KVRETUR    TO W-KVRETUR61-W                          
009611           ELSE                                                           
009612              ADD +1            TO W-ANTALPOSTER61                        
009613              ADD IN-KVARBDAG   TO W-KVARBDAG61                           
009614              ADD IN-KVRETUR    TO W-KVRETUR61                            
009615           END-IF                                                         
009616         WHEN NDC-AU                                                      
009617           IF IN-FLBYTGAR = 'J'                                           
009618              ADD +1            TO W-ANTALPOSTER62-W                      
009619              ADD IN-KVARBDAG   TO W-KVARBDAG62-W                         
009620              ADD IN-KVRETUR    TO W-KVRETUR62-W                          
009621           ELSE                                                           
009622              ADD +1            TO W-ANTALPOSTER62                        
009623              ADD IN-KVARBDAG   TO W-KVARBDAG62                           
009624              ADD IN-KVRETUR    TO W-KVRETUR62                            
009625           END-IF                                                         
009626     END-EVALUATE                                                         
009627                                                                          
009628     .                                                                    
009629     EJECT                                                                
009630                                                                          
009652 C-BERAKNA SECTION.                                                       
009653                                                                          
009654     IF W-ANTALPOSTER > ZERO                                              
009655        COMPUTE W-ANTALSUMMA  =  W-KVARBDAG /  W-ANTALPOSTER              
009656     END-IF                                                               
009657     IF W-ANTALPOSTER-W > ZERO                                            
009658        COMPUTE W-ANTALSUMMA-W = W-KVARBDAG-W / W-ANTALPOSTER-W           
009659     END-IF                                                               
009660     IF W-ANTALPOSTER2 > ZERO                                             
009661        COMPUTE W-ANTALSUMMA2 =  W-KVARBDAG2 / W-ANTALPOSTER2             
009662     END-IF                                                               
009666     IF W-ANTALPOSTER41 > ZERO                                            
009667        COMPUTE W-ANTALSUMMA41 =  W-KVARBDAG41 / W-ANTALPOSTER41          
009668     END-IF                                                               
009669     IF W-ANTALPOSTER41-W > ZERO                                          
009670        COMPUTE W-ANTALSUMMA41-W =                                        
009671                        W-KVARBDAG41-W / W-ANTALPOSTER41-W                
009672     END-IF                                                               
009673     IF W-ANTALPOSTER42 > ZERO                                            
009674        COMPUTE W-ANTALSUMMA42 =  W-KVARBDAG42 / W-ANTALPOSTER42          
009675     END-IF                                                               
009676     IF W-ANTALPOSTER43 > ZERO                                            
009677        COMPUTE W-ANTALSUMMA43 =  W-KVARBDAG43 / W-ANTALPOSTER43          
009678     END-IF                                                               
009679     IF W-ANTALPOSTER5 > ZERO                                             
009680        COMPUTE W-ANTALSUMMA5 =  W-KVARBDAG5 / W-ANTALPOSTER5             
009681     END-IF                                                               
009682     IF W-ANTALPOSTER5-W > ZERO                                           
009683        COMPUTE W-ANTALSUMMA5-W = W-KVARBDAG5-W / W-ANTALPOSTER5-W        
009684     END-IF                                                               
009685     IF W-ANTALPOSTER61 > ZERO                                            
009686        COMPUTE W-ANTALSUMMA61 =  W-KVARBDAG61 / W-ANTALPOSTER61          
009687     END-IF                                                               
009688     IF W-ANTALPOSTER61-W > ZERO                                          
009689        COMPUTE W-ANTALSUMMA61-W =                                        
009690                        W-KVARBDAG61-W / W-ANTALPOSTER61-W                
009691     END-IF                                                               
009692     IF W-ANTALPOSTER62 > ZERO                                            
009693        COMPUTE W-ANTALSUMMA62 =  W-KVARBDAG62 / W-ANTALPOSTER62          
009694     END-IF                                                               
009695     IF W-ANTALPOSTER62-W > ZERO                                          
009696        COMPUTE W-ANTALSUMMA62-W =                                        
009697                        W-KVARBDAG62-W / W-ANTALPOSTER62-W                
009698     END-IF                                                               
009699                                                                          
009700     MOVE W-ANTALSUMMA       TO W001R2-ANTAL                              
009701     MOVE W-ANTALSUMMA-W     TO W001R2-ANTAL-W                            
009702     MOVE W-ANTALSUMMA2      TO W001R2-ANTAL-91                           
009703     MOVE W-ANTALSUMMA41     TO W001R2-ANTAL-41                           
009704     MOVE W-ANTALSUMMA41-W   TO W001R2-ANTAL-41-W                         
009705     MOVE W-ANTALSUMMA42     TO W001R2-ANTAL-42                           
009706     MOVE W-ANTALSUMMA43     TO W001R2-ANTAL-43                           
009707     MOVE W-ANTALSUMMA5      TO W001R2-ANTAL-51                           
009708     MOVE W-ANTALSUMMA5-W    TO W001R2-ANTAL-51-W                         
009709     MOVE W-ANTALSUMMA61     TO W001R2-ANTAL-61                           
009710     MOVE W-ANTALSUMMA61-W   TO W001R2-ANTAL-61-W                         
009711     MOVE W-ANTALSUMMA62     TO W001R2-ANTAL-62                           
009712     MOVE W-ANTALSUMMA62-W   TO W001R2-ANTAL-62-W                         
009713     MOVE W-KVRETUR11        TO W001R2-KVRETUR-11                         
009714     MOVE W-KVARBDAG2        TO W001R2-WORKDAY-91                         
009715     MOVE W-ANTALPOSTER2     TO W001R2-TOTREPT-91                         
009716     MOVE W-KVRETUR41        TO W001R2-KVRETUR-41                         
009717     MOVE W-KVRETUR41-W      TO W001R2-KVRETUR-41-W                       
009718     MOVE W-KVRETUR42        TO W001R2-KVRETUR-42                         
009719     MOVE W-KVRETUR43        TO W001R2-KVRETUR-43                         
009720     MOVE W-KVRETUR51        TO W001R2-KVRETUR-51                         
009721     MOVE W-KVRETUR51-W      TO W001R2-KVRETUR-51-W                       
009722     MOVE W-KVRETUR61        TO W001R2-KVRETUR-61                         
009723     MOVE W-KVRETUR61-W      TO W001R2-KVRETUR-61-W                       
009724     MOVE W-KVRETUR62        TO W001R2-KVRETUR-62                         
009725     MOVE W-KVRETUR62-W      TO W001R2-KVRETUR-62-W                       
009726                                                                          
009727                                                                          
009728***** RUBRIKRAD 3 ******                                                  
009729                                                                          
009730     MOVE W001R2-DELRUBRIK-2 TO UT-RAD                                    
009731     PERFORM S02-SKRIV-UT-RAD                                             
009732                                                                          
009733     MOVE W001R2-DELRUBRIK-2-91 TO UT-RAD-91                              
009734     PERFORM S03-SKRIV-UT-RAD-91                                          
009735                                                                          
009736     MOVE W001R2-DELRUBRIK-2-51 TO UT-RAD-51                              
009737     PERFORM S04-SKRIV-UT-RAD-51                                          
009738                                                                          
009739     MOVE W001R2-DELRUBRIK-2-41 TO UT-RAD-41                              
009740     PERFORM S05-SKRIV-UT-RAD-41                                          
009741                                                                          
009742     MOVE W001R2-DELRUBRIK-2-42 TO UT-RAD-42                              
009743     PERFORM S06-SKRIV-UT-RAD-42                                          
009744                                                                          
009745     MOVE W001R2-DELRUBRIK-2-43 TO UT-RAD-43                              
009746     PERFORM S07-SKRIV-UT-RAD-43                                          
009747                                                                          
009749     MOVE W001R2-DELRUBRIK-2-61 TO UT-RAD-61                              
009750     PERFORM S08-SKRIV-UT-RAD-61                                          
009751                                                                          
009753     MOVE W001R2-DELRUBRIK-2-62 TO UT-RAD-62                              
009754     PERFORM S09-SKRIV-UT-RAD-62                                          
009755                                                                          
009756     .                                                                    
009760     EJECT                                                                
009891 Z-FINIT SECTION.                                                         
009892     CLOSE W3714D                                                         
009893           LISTA                                                          
009894           LISTB                                                          
009895           LISTC                                                          
009896           LISTD                                                          
009897           LISTE                                                          
009898           LISTF                                                          
009899           LISTG                                                          
009900           LISTH                                                          
009901     SKIP2                                                                
009902     MOVE 'S' TO POSTSUM-OPKOD                                            
009910     CALL POSTSUM USING POSTSUM-PARM                                      
010000     .                                                                    
010001     EJECT                                                                
010002 S01-LAES-W3714D  SECTION.                                                
010003     READ W3714D INTO IN-AREA                                             
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA                                        
010006        SET END-OF-W3714D TO TRUE                                         
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W3714D' TO POSTSUM-FDNAMN                                   
010010        MOVE 'W37154D1' TO POSTSUM-DDNAMN2                                
010013        MOVE ' UT '    TO POSTSUM-TRANSTYP                                
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010020     .                                                                    
010201     EJECT                                                                
010202 S02-SKRIV-UT-RAD SECTION.                                                
010204     SKIP2                                                                
010205                                                                          
010206     WRITE LISTAS FROM UT-RAD                                             
010207                                                                          
010208     .                                                                    
010209     EJECT                                                                
010210 S03-SKRIV-UT-RAD-91 SECTION.                                             
010212     SKIP2                                                                
010220                                                                          
010230     WRITE LISTBS FROM UT-RAD-91                                          
010240                                                                          
010250     .                                                                    
010260     EJECT                                                                
010270 S04-SKRIV-UT-RAD-51 SECTION.                                             
010280     SKIP2                                                                
010290                                                                          
010300     WRITE LISTCS FROM UT-RAD-51                                          
010310                                                                          
010320     .                                                                    
010330     EJECT                                                                
010340 S05-SKRIV-UT-RAD-41 SECTION.                                             
010360     SKIP2                                                                
010370                                                                          
010380     WRITE LISTDS FROM UT-RAD-41                                          
010390                                                                          
010391     .                                                                    
010392     EJECT                                                                
010393 S06-SKRIV-UT-RAD-42 SECTION.                                             
010395     SKIP2                                                                
010396                                                                          
010397     WRITE LISTES FROM UT-RAD-42                                          
010398                                                                          
010399     .                                                                    
010400     EJECT                                                                
010401 S07-SKRIV-UT-RAD-43 SECTION.                                             
010402     SKIP2                                                                
010404                                                                          
010405     WRITE LISTFS FROM UT-RAD-43                                          
010406                                                                          
010407     .                                                                    
010408     EJECT                                                                
010409 S08-SKRIV-UT-RAD-61 SECTION.                                             
010410     SKIP2                                                                
010411                                                                          
010412     WRITE LISTGS FROM UT-RAD-61                                          
010413                                                                          
010414     .                                                                    
010415     EJECT                                                                
010416 S09-SKRIV-UT-RAD-62 SECTION.                                             
010417     SKIP2                                                                
010418                                                                          
010419     WRITE LISTHS FROM UT-RAD-62                                          
010420                                                                          
010421     .                                                                    
010422     EJECT                                                                
010430 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
