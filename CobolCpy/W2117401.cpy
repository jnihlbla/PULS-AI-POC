000100 01  W2117401.                                                            
000200*                                 BELÄGGNING AK                           
000300*                                 (FIL TILL ICU)                          
000400*                                 PTYP 001 - UPPACKNING                   
000500*                                 PTYP 002 - UPP / KVAL                   
000600*                                 PTYP 003 - UPP / FÖRP                   
000700*                                 PTYP 004 - UPP / KVAL / FÖRP            
000800*                                 PTYP 005 - MÅLERI                       
000900*                                 PTYP 006 - SATSER                       
001000*                                 PTYP 007 - FRÅN C2                      
001100*                                 PTYP 008 - RETURER                      
001200*                                 PTYP 009 - CLEARING                     
001300     03 TIAAVV               PIC 9(4).                                    
001400*                                 ÅR - VECKA  (ÅÅVV)                      
001500     03 KDCLAGER             PIC 9.                                       
001600*                                 CENTRALLAGERKOD                         
001700     03 IDPTYP               PIC X(3).                                    
001800*                                 POSTTYP                                 
001900     03 KVANTAL-R31          PIC 9(6).                                    
002000*                                 ANTAL ALLMÄNT                           
002100     03 SUAKSV-AKS           PIC 9(9)V9(2).                               
002200*                                 VÄRDE AV ANKOMSTSALDO                   
002300     03 SUAKSV-AKF           PIC 9(9)V9(2).                               
002400*                                 VÄRDE AV ANKOMSTSALDO                   
002500     03 TIDAG-G              PIC 9(2)V9(2).                               
002600     03 TIDAG-GP             PIC 9(2)V9(2).                               
002700     03 SUAKSV-RO            PIC 9(9)V9(2).                               
002800*                                 VÄRDE AV ANKOMSTSALDO                   
002900     03 SUAKSV-R32           PIC 9(9)V9(2).                               
003000*                                 VÄRDE AV ANKOMSTSALDO                   
003100     03 KVANTAL-R32          PIC 9(6).                                    
003200*                                 ANTAL ALLMÄNT                           
003300*** END COPY W2117401C0  LENGTH=72                                        
