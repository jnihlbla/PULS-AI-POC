000100 01  W22242-CTX.                                                          
000200*                                 COPYTEXT TILL FILEN W22242,             
000300*                                 LARM STORA ELLER SM≈                    
000400*                                 UTTAG FR≈N SDC OCH NDC                  
000500*                                 LARM A = H÷G ORDERING≈NG                
000600*                                          SISTA 2 VECKORNA               
000700*                                          OCH   6 VECKORNA               
000800*                                 LARM B = H÷G ORDERING≈NG                
000900*                                          SISTA 2 VECKORNA               
001000*                                 LARM C = H÷G ORDERING≈NG                
001100*                                          SISTA 6 VECKORNA               
001200*                                 LARM D = L≈G ORDERING≈NG                
001300*                                          SISTA 6 VECKORNA               
001400     03 IDANSK               PIC S9(3)           COMP-3.                  
001500*                                 ANSKAFFARNUMMER                         
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 LARM-A               PIC X.                                       
001900     03 LARM-B               PIC X.                                       
002000     03 LARM-C               PIC X.                                       
002100     03 LARM-D               PIC X.                                       
002200     03 TIAAAA               PIC 9(4).                                    
002300*                                 ≈RTAL (≈≈≈≈)                            
002400     03 TIVV                 PIC S9(3)           COMP-3.                  
002500*                                 VECKA  (VV)                             
002600     03 KDVVKL               PIC S9              COMP-3.                  
002700*                                 VOLYMVƒRDESKLASS                        
002800     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
002900*                                 PERIODBEHOV REFILLING                   
003000     03 KVPB-2V              PIC S9(6)V9(1)      COMP-3.                  
003100*                                 PERIODBEHOV REFILLING                   
003200     03 KVPB-4V              PIC S9(6)V9(1)      COMP-3.                  
003300*                                 PERIODBEHOV REFILLING                   
003400     03 OI-TOT-2V            PIC S9(7)           COMP-3.                  
003500     03 OI-TOT-4V            PIC S9(7)           COMP-3.                  
003600*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
