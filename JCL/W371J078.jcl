//W371J078 JOB (670W3710100W371J078,W100),'RTN W371S5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P078                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W371.W371S5.W37178(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S5.W37178(+1)                                    
//SYSIN           DD *                                                          
W37178-001                                                                      
&IDDC                                                                           
//    ENDIF                                                                     
//EMPTY2 EXEC WEMPTST,DSIN=W371.W371S5.W3717C(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W371.W371S5.W3717C(+1)                                    
//SYSIN           DD *                                                          
CORE-PROFORMA                                                                   
&IDDC.&IDUSER.                                                                  
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W371J078                                         
