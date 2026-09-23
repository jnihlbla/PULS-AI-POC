000100 01  6018-W6GX6018.                                                       
000200*                                 INLEVERANS LÖPNUMMERSERIE               
000300*                                 FYSISK NYCKEL: KDSEGKEY 1               
000400     03 6018-KDSEGKEY        PIC X.                                       
000500*                                 TEKNISK SEGMENT-NYCKEL                  
000600*                                 TECHNICAL SEGMENT KEY                   
000700     03 6018-IDLOPNRM        PIC S9(9)           COMP-3.                  
000800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000900*                                 (0VVDLLLLK)                             
001000*                                 SERIAL NO RECEIVING REPORT              
001100*                                 (0WWDLLLLC)                             
001200     03 6018-IDOKOLLI        PIC 9(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400*                                 ODETTE CASE NUMBER                      
001500     03 6018-IDFS            PIC X(8).                                    
001600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001700*                                 ADVICE NOTE NUMBER ODETTE               
001800     03 6018-IDILIST         PIC 9(5).                                    
001900*                                 INLÄGGNINGSLISTEIDENTITET               
002000*                                 REPORTINGLIST-IDENTITY                  
002100     03 6018-IDLOPNRM-NDC    PIC S9(9)           COMP-3.                  
002200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002300*                                 (0VVDLLLLK)                             
002400*                                 SERIAL NO RECEIVING REPORT              
002500*                                 (0WWDLLLLC)                             
002600     03 6018-IDLOPNRM-JP-AU  PIC S9(9)           COMP-3.                  
002700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002800*                                 (0VVDLLLLK)                             
002900*                                 SERIAL NO RECEIVING REPORT              
003000*                                 (0WWDLLLLC)                             
003100     03 6018-IDLOPNRM-RET    PIC S9(9)           COMP-3.                  
003200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003300*                                 (0VVDLLLLK)                             
003400*                                 SERIAL NO RECEIVING REPORT              
003500*                                 (0WWDLLLLC)                             
003600     03 6018-IDLOPNRM-NDC-RET                                             
003700                             PIC S9(9)           COMP-3.                  
003800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003900*                                 (0VVDLLLLK)                             
004000*                                 SERIAL NO RECEIVING REPORT              
004100*                                 (0WWDLLLLC)                             
004200     03 6018-IDLOPNRM-JP-AU-RET                                           
004300                             PIC S9(9)           COMP-3.                  
004400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004500*                                 (0VVDLLLLK)                             
004600*                                 SERIAL NO RECEIVING REPORT              
004700*                                 (0WWDLLLLC)                             
004800     03 6018-IDLOPNRM-SDC-RET                                             
004900                             PIC S9(9)           COMP-3.                  
005000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005100*                                 (0VVDLLLLK)                             
005200*                                 SERIAL NO RECEIVING REPORT              
005300*                                 (0WWDLLLLC)                             
005400     03 6018-IDTRPTNR-INLEV  PIC S9(5)           COMP-3.                  
005500*                                 TRANSPORTIDENTITET INLEVERANS           
005600*                                 TRANSPORT IDENTITY GOODS RECIEV         
005700*                                 ING                                     
005800     03 6018-IDTRPTNR-INTFL  PIC S9(5)           COMP-3.                  
005900*                                 TRANSPORTIDENTITET INTERNTRANSP         
006000*                                 ORT                                     
006100*                                 TRANSPORT IDENTITY INTERNAL TRA         
006200*                                 NSPORT                                  
006300     03 FILLER               PIC X(19).                                   
006400*** END OF VILMAII-COPY LENGTH= 83 BYTES                                  
