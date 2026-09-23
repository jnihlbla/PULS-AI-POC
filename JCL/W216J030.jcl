//W216J030 JOB (670W2160100W216J030,W100),'RTN W216S1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W216    EXEC W216P030                                                         
//W21630.W21630D1 DD *                                                          
&URVAL1                                                                         
&URVAL2                                                                         
&URVAL3                                                                         
&URVAL4                                                                         
&URVAL5                                                                         
&URVAL6                                                                         
&URVAL7                                                                         
&URVAL8                                                                         
&URVAL9                                                                         
&URVAL10                                                                        
&URVAL11                                                                        
&URVAL12                                                                        
&URVAL13                                                                        
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W216J030                                         
