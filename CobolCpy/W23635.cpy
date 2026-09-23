000100 01  W23635.                                                              
000200*                                                                         
000300*                                 ARTIKLAR SOM BLIVIT                     
000400*                                 INLEVERERADE GÅGNA VECKAN               
000500*                                                                         
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
001400*                                 AVISERINGSDATUM (YYMMDD)                
001500     03 KVAVIS               PIC S9(7)           COMP-3.                  
001600*                                 AVISERAT ANTAL                          
001700     03 TIUPPDAT             PIC S9(7)           COMP-3.                  
001800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001900     03 KDEFFMAN             PIC X.                                       
002000*                                 EMIL-KOD                                
002100     03 KVDAGAR-TT           PIC S9(3)           COMP-3.                  
002200*                                 DAGAR TULL- OCH TRANSPORT-TID           
002300     03 KVDAGAR-INLEV        PIC S9(3)           COMP-3.                  
002400*                                 INLEVERANSTID     (ANTAL DAGAR)         
002500     03 BEFT                 PIC S9(3)           COMP-3.                  
002600*                                 FÖRPACKNINGSTYP                         
002700     03 IDANSK               PIC S9(3)           COMP-3.                  
002800*                                 ANSKAFFARNUMMER                         
002900     03 DAAVROP-AVS          PIC 9(6).                                    
003000*                                 AVSÄNDNINGSVECKA (PLANERAD)             
003100*                                 (ÅÅÅÅVV)                                
003200     03 TILEVDAG             PIC S9              COMP-3.                  
003300*                                 AVSÄNDNINGSDAG INOM VECKA               
003400     03 KVAVROP-AVB          PIC S9(7)           COMP-3.                  
003500*                                 AVBOKAT ANTAL                           
003600*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
