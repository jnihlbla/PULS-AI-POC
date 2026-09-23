000100 01  PROP-WZ04PROP.                                                       
000200*                                 RECORD THAT CONTAINS VARIOUS            
000300*                                 PROPERTIES FOR USE IN GENERIC           
000400*                                 MODULES OF WZ04                         
000500     03 PROP-IDPTYP          PIC X(6)                                     
000600                             VALUE '¤PROP'.                               
000700     03 PROP-KVANTAL         PIC 9(3)                                     
000800                             VALUE 0.                                     
000900*                                 NUMBER                                  
001000     03 PROP-TAB             OCCURS 0 TO 10 TIMES                         
001100                             DEPENDING ON PROP-KVANTAL                    
001200                             INDEXED PROP-IX.                             
001300*                                 PROPERTY TABLE                          
001400        05 PROP-IDPROPTYPE   PIC X(15)                                    
001500                             VALUE SPACES.                                
001600*                                 PROPERTY TYPE                           
001700        05 PROP-IDPROPNAME   PIC X(100)                                   
001800                             VALUE SPACES.                                
001900*                                 PROPERTY NAME                           
002000        05 PROP-BEPROPVALUE  PIC X(100)                                   
002100                             VALUE SPACES.                                
002200*                                 PROPERTY NAME                           
002300**** END OF VILMAII-COPY LENGTH= 2159 BYTES                               
