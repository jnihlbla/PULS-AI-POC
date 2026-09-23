//W479J066 JOB (650W4790100W479J066,W100),'RTN W479V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W47966   EXEC W479P066,                                                       
//             INDIN=W479.W479V2,GEN='(+0)',                                    
//             INDUT=W479.W479V2                                                
//*                                                                             
//W47966.SYSIN    DD *                                                          
W47966-001                                                                      
//*                                                                             
//*                                                                             
//***************************************************************               
//*  THIS PROCEDURE WILL PROCESS MULTIPLE REPORTS FROM THE                      
//*  INPUT FILE W479.W479V2.W47968C                                             
//*                                                                             
//*  THE CONTROL INFORMATION CONSISTS OF 2 "HEADER" RECORDS                     
//*  BEFORE THE ACTUAL REPORT DATA, EACH RECORD STARTING WITH THE               
//*  PREFIX "¤DAP" AND FOLLOWED BY:                                             
//*                                                                             
//*    EXAMPLE -                                                                
//*    RCD 1: " ¤DAPINVDISTR-W                                                  
//*    RCD 2: " ¤DAP1AW47966-001"  WHERE 01A IS IDDC                            
//*                                                                             
//***************************************************************               
//EMPTY1 EXEC WEMPTST,DSIN=W479.W479V2.W47968C(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W479.W479V2.W47968C(+1)                                   
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J066                                         
