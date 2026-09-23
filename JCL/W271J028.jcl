//W271J028 JOB (640W2710100W271J028,W100),'RTN W271B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P028                                                         
//*                                                                             
//W27128.W27128D1 DD *                                                          
&URVAL1.                                                                        
&URVAL2.                                                                        
&URVAL3.                                                                        
/*                                                                              
//*                                                                             
// IF W271.W27128.RC = 0 THEN                                                   
//SOPSET EXEC WSOP                                                              
   SET VALUE W271B2                                                             
      BEST(OK)                                                                  
   END-SET                                                                      
// ELSE                                                                         
//SOPSET2 EXEC WSOP                                                             
   SET VALUE W271B2                                                             
      BEST(FULL)                                                                
   END-SET                                                                      
//*                                                                             
// ENDIF                                                                        
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J028                                         
