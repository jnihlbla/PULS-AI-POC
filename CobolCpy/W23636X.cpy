000100 01  W23636X.                                                             
000200*                                                                         
000300*                                 ARTIKLAR SOM BLIVIT                     
000400*                                 INLEVERERADE G≈GNA VECKAN               
000500*                                                                         
000600     03 IDARTNR              PIC Z(7)9                                    
000700                             VALUE ZEROS.                                 
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 IDLEVNR              PIC X(5)                                     
001100                             VALUE SPACES.                                
001200*                                 LEVERANT÷RNUMMER                        
001300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001400     03 IDLOPNRM             PIC Z(7)9                                    
001500                             VALUE ZEROS.                                 
001600*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
001700*                                 (0VVDLLLLK)                             
001800*                                 SERIAL NO RECEIVING REPORT              
001900*                                 (0WWDLLLLC)                             
002000     03 TIAVIDAT             PIC 9(6)                                     
002100                             VALUE ZEROS.                                 
002200*                                 AVISERINGSDATUM (YYMMDD)                
002300*                                 ADVICE NOTE DATE                        
002400     03 KVAVIS               PIC Z(5)9                                    
002500                             VALUE ZEROS.                                 
002600*                                 AVISERAT ANTAL                          
002700*                                 QUANTITY NOTIFIED                       
002800     03 TIUPPDAT             PIC 9(6)                                     
002900                             VALUE ZEROS.                                 
003000*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
003100*                                 UPDATING DATE     (YYMMDD)              
003200     03 KDEFFMAN             PIC X                                        
003300                             VALUE SPACE.                                 
003400*                                 EMIL-KOD                                
003500*                                 EMIL-CODE                               
003600     03 KVDAGAR-TT           PIC Z9                                       
003700                             VALUE ZEROS.                                 
003800*                                 DAGAR TULL- OCH TRANSPORT-TID           
003900     03 KVDAGAR-INLEV        PIC Z9                                       
004000                             VALUE ZEROS.                                 
004100*                                 INLEVERANSTID     (ANTAL DAGAR)         
004200     03 BEFT                 PIC Z9                                       
004300                             VALUE ZEROS.                                 
004400*                                 F÷RPACKNINGSTYP                         
004500*                                 PACKAGING TYPE                          
004600     03 IDANSK               PIC Z(2)9                                    
004700                             VALUE ZEROS.                                 
004800*                                 ANSKAFFARNUMMER                         
004900*                                 PROCURER NO.                            
005000     03 DAAVROP-AVS          PIC 9(6)                                     
005100                             VALUE ZEROS.                                 
005200*                                 AVSƒNDNINGSVECKA (PLANERAD)             
005300*                                 (≈≈≈≈VV)                                
005400     03 TILEVDAG             PIC 9                                        
005500                             VALUE ZERO.                                  
005600*                                 AVSƒNDNINGSDAG INOM VECKA               
005700*                                 DELIVERY WEEK DAY                       
005800     03 KVAVROP-AVB          PIC Z(6)9                                    
005900                             VALUE ZEROS.                                 
006000*                                 AVBOKAT ANTAL                           
006100     03 TIAVROP-AAMMDD       PIC X(6)                                     
006200                             VALUE SPACES.                                
006300*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
006400*                                 YEAR - MONTH - DAY  (YYMMDD)            
006500     03 KVDAGAR              PIC -9(3)                                    
006600                             VALUE ZEROS.                                 
006700*                                 ANTAL DAGAR                             
006800*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
