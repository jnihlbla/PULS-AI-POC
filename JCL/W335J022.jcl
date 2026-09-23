//W335J022 JOB (670W3350100W335J022,W100),'RTN W335S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
//* IDUSER= &IDUSER                                                             
//* UTSKRIFT= &UTSKRIFT                                                         
//* SORT= &SORT.                                                                
//* URVAL1= &URVAL1                                                             
//* URVAL2= &URVAL2                                                             
//*                                                                             
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W335    EXEC W335P022                                                         
//*                                                                             
//W33522.SYSIN    DD *                                                          
&IDUSER.&UTSKRIFT.&SORT.                                                        
&URVAL1.                                                                        
&URVAL2.                                                                        
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W335.W335S1.W33522(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W335.W335S1.W33522(+1),CPU=3                              
//SYSIN           DD *                                                          
&IDUSER                                                                         
&UTSKRIFT.W33522                                                                
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J022                                         
