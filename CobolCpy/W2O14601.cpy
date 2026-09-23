000100 01  MOD-W2O14601.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-TIFINLV-FOM-IN   PIC X(5).                                    
000700*                                 DATE GOODS REC-FROM,(YYWWD D=1)         
000800     03 MOD-TIFINLV-TOM-IN   PIC X(5).                                    
000900*                                 DATE GOODS REC-UNTIL(YYWWD D=1)         
001000     03 MOD-IDPROJ-IN        PIC X(4).                                    
001100*                                 PARTS PROJECT IDENTITY                  
001200     03 MOD-TIFINLV-FOM-UT   PIC X(5).                                    
001300*                                 DATE GOODS REC-FROM,(YYWWD D=1)         
001400     03 MOD-TIFINLV-TOM-UT   PIC X(5).                                    
001500*                                 DATE GOODS REC-UNTIL(YYWWD D=1)         
001600     03 MOD-IDPROJ-UT        PIC X(4).                                    
001700*                                 PARTS PROJECT IDENTITY                  
001800     03 MOD-KVANTAL-TOT      PIC Z(2)9.                                   
001900     03 MOD-KVANTAL-INLEV    PIC Z(2)9.                                   
002000     03 MOD-KVANTAL-PISK     PIC Z(2)9.                                   
002100     03 MOD-OUTPUT           OCCURS 11 TIMES.                             
002200        05 MOD-IDARTNR       PIC Z(7)9.                                   
002300*                                 PART NUMBER                             
002400        05 MOD-IDPROJ        PIC X(4).                                    
002500*                                 PARTS PROJECT IDENTITY                  
002600        05 MOD-KDEMBKOD      PIC Z(2)9.                                   
002700        05 MOD-TIFINLV       PIC 9(5).                                    
002800*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
002900        05 MOD-FLPISK        PIC X.                                       
003000*                                 FAST PART (PISK)                        
003100        05 MOD-TIREGDAT      PIC 9(6).                                    
003200*                                 REGISTRATION DATE (YYMMDD)              
003300        05 MOD-IDNAMN        PIC X(40).                                   
003400     03 MOD-TEMFSINF         PIC X(55).                                   
003500*                                 INFORMATION MESSAGE                     
003600*** END OF VILMAII-COPY LENGTH= 873 BYTES                                 
