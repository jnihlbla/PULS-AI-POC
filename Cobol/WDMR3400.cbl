000100 Id Division.                                                             
000200     skip2                                                                
000300 Program-Id.     WDMR3400.                                                
000400*AUTHOR.         ODD OLSEN.                                               
000500*DATE-WRITTEN.   92/04/02.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPA LADDFIL FÖR DATA MANAGER                                   
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     skip3                                                                
001900 Environment Division.                                                    
002000     skip2                                                                
002100 Input-Output Section.                                                    
002200                                                                          
002300 File-Control.                                                            
002400     skip2                                                                
002500*          --- STYRFIL                                                    
002600     Select WDMR33                     Assign To WDMR34D1.                
002700     skip2                                                                
002800*          --- LADDFIL FÖR DATAMANAGER                                    
002900     Select WDMR34                     Assign To WDMR34D2.                
003000     eject                                                                
003100 Data Division.                                                           
003200     skip3                                                                
003300 File Section.                                                            
003400     skip3                                                                
003500 Fd  WDMR33                                                               
003600     Recording       F                                                    
003700     Block Contains  0.                                                   
003800     skip2                                                                
003900 01  Filler                      Pic X(21).                               
004000     skip3                                                                
004100 Fd  WDMR34                                                               
004200     Recording       F                                                    
004300     Block Contains  0.                                                   
004400     skip2                                                                
004500 01  ut-post                     Pic X(80).                               
004600     eject                                                                
004700 Working-Storage Section.                                                 
004701                                                                          
004710*    -- CHECKED BY WY2000                                                 
004800     skip2                                                                
004900 77  idpgm                       Pic X(8)    Value 'WDMR3400'.            
005000 77  ja                          Pic X       Value 'J'.                   
005100 77  nej                         Pic X       Value 'N'.                   
005200 77  del-pgm                     Pic X       Value 'D'.                   
005300 77  new-pgm                     Pic X       Value 'N'.                   
005400                                                                          
005500 77  eof-sw                      Pic X       Value 'N'.                   
005600     88  eof                                 Value 'J'.                   
005700     eject                                                                
005800* --- Arbetsareor för redigering av utfil                                 
005900 01  command.                                                             
006000     03                          Pic X(08) Value 'REPLACE'.               
006100     03 cmd-member               Pic X(50).                               
006200     03                          Pic X(01) Value '.'.                     
006300 01  rub-pgm                     Pic X(07) Value 'PROGRAM'.               
006400 01  rub-catalog                 Pic X(50)                                
006500       Value 'CATALOG ''PROGRAM'' '.                                      
006600 01  rub-contains                Pic X(08) Value 'CONTAINS'.              
006700 01  module                      Pic X(08).                               
006800 01  obsolete                    Pic X(50)                                
006900       Value 'OBSOLETE-DEF CATALOG ''REMOVE'' '.                          
006910 01  cmd-remove.                                                          
006920     03                          Pic X(08) Value 'REMOVE '.               
006930     03 rem-member               Pic X(50).                               
006940     03                          Pic X(01) Value ' '.                     
007000     eject                                                                
007100 01  dagens-datum                Pic 9(6)    Value Zero.                  
007200 01  Filler Redefines dagens-datum.                                       
007300     03  dagens-datum-aar        Pic 9(2).                                
007400     03  dagens-datum-maanad     Pic 9(2).                                
007500     03  dagens-datum-dag        Pic 9(2).                                
007600     eject                                                                
007700 01  dynamiska-subprogram.                                                
007800*                                                                         
007900     03  abend                   Pic X(8)    Value 'ABEND'.               
008000     03  postsum                 Pic X(8)    Value 'POSTSUM'.             
008100     skip2                                                                
008200*    --- PARAMETRAR TILL ABEND                                            
008300                                                                          
008400 77  rkod-abend-utan-dump        Pic S9(4)   Comp Value +16.              
008500 77  rkod-abend-med-dump         Pic S9(4)   Comp Value +1000.            
008600     skip2                                                                
008700 01  feltext.                                                             
008800     03  Filler                  Pic X(8)    Value 'FELTEXT'.             
008900     03  feltext-str             Pic X(72)   Value Space.                 
009000     eject                                                                
009100*    --- PARAMETRAR TILL POSTSUM                                          
009200*                                                                         
009300*01  -COPY W0005   -PRE  POSTSUM-                                         
009400     eject                                                                
009500 01  in-area-start               Pic X(24)   Value                        
009600                                 'IN-AREA-START  '.                       
009700     skip2                                                                
009800 01  in-area.                                                             
009900     03 in-styr                  Pic X.                                   
010000     03 in-pgm                   Pic X(8).                                
010100     eject                                                                
010200 01  ut-area-start               Pic X(24)   Value                        
010300                                 'UT-AREA-START  '.                       
010400     skip2                                                                
010500 01  ut-area                     Pic X(80).                               
010600     eject                                                                
010700 Procedure Division.                                                      
010800     skip2                                                                
010900 STYR Section.                                                            
011000     Perform A-INIT                                                       
011100     Perform S01-LAES-WDMR33                                              
011200     Perform Until eof                                                    
011300       Move Space to cmd-member                                           
011400       String in-pgm Delimited By Space                                   
011500              '-PGM' Delimited By Size                                    
011600         Into cmd-member                                                  
011610       Move cmd-member To rem-member                                      
011700       Evaluate in-styr                                                   
011800         When new-pgm                                                     
011900           Move command To ut-area                                        
012000           Perform S11-SKRIV-WDMR34                                       
012300           Move rub-pgm To ut-area                                        
012400           Perform S11-SKRIV-WDMR34                                       
012500           Move rub-catalog To ut-area                                    
012600           Perform S11-SKRIV-WDMR34                                       
012700           Move rub-contains To ut-area                                   
012800           Perform S11-SKRIV-WDMR34                                       
012900           Evaluate in-pgm(7:2) = Space                                   
013000               Also in-pgm(6:1) = Space                                   
013100               Also in-pgm(1:1) = 'W'                                     
013110               Also in-pgm Alphabetic                                     
013200             When Any  Also True  Also Any  Also Any                      
013300               Move in-pgm To module                                      
013400             When True Also False Also True Also Any                      
013500               Move Space to module                                       
013600               String in-pgm Delimited By Space                           
013700                      '00'   Delimited By Size                            
013800                 Into module                                              
013900             When Other                                                   
014000               Move in-pgm To module                                      
014100           End-Evaluate                                                   
014101           Move module To ut-area                                         
014110           Perform S11-SKRIV-WDMR34                                       
014200           Move '.' To ut-area                                            
014300           Perform S11-SKRIV-WDMR34                                       
014400         When del-pgm                                                     
014401           Move cmd-remove To ut-area                                     
014402           Perform S11-SKRIV-WDMR34                                       
014430           Move '.' To ut-area                                            
014440           Perform S11-SKRIV-WDMR34                                       
014450         When Other                                                       
014460           Continue                                                       
014700       End-Evaluate                                                       
014800       Perform S01-LAES-WDMR33                                            
014900     End-Perform                                                          
015000                                                                          
015100     Perform Z-FINIT                                                      
015200                                                                          
015300     Move Zero To return-code                                             
015400     Goback                                                               
015500     .                                                                    
015600     eject                                                                
015700 A-INIT Section.                                                          
015800                                                                          
015900     Open Input  WDMR33                                                   
016000                                                                          
016100     Open Output WDMR34                                                   
016200     skip2                                                                
016300     Accept dagens-datum  From Date                                       
016400     Move idpgm To postsum-prognamn                                       
016500     .                                                                    
016600     eject                                                                
016700 Z-FINIT Section.                                                         
016800     Close WDMR33                                                         
016900           WDMR34                                                         
017000     skip2                                                                
017100     Move 'S' To postsum-opkod                                            
017200     Call POSTSUM Using postsum-parm                                      
017300     .                                                                    
017400     eject                                                                
017500 S01-LAES-WDMR33  Section.                                                
017600     skip2                                                                
017700     Read WDMR33 Into in-area                                             
017800     At End                                                               
017900        Set eof To True                                                   
018100     Not At End                                                           
018200        Move 'WDMR33'   To postsum-fdnamn                                 
018300        Move 'WDMR34D1' To postsum-ddnamn2                                
018400        Move Space      To postsum-transtyp                               
018500        Call POSTSUM Using postsum-parm                                   
018600     End-Read                                                             
018700     .                                                                    
018800     eject                                                                
018900 S11-SKRIV-WDMR34 Section.                                                
019000     skip2                                                                
019100     Write ut-post From ut-area                                           
019200                                                                          
019300     Move Space      To postsum-transtyp                                  
019400     Move 'WDMR34'   To postsum-fdnamn                                    
019500     Move 'WDMR34D2' To postsum-ddnamn2                                   
019600     Call POSTSUM Using postsum-parm                                      
019700     .                                                                    
